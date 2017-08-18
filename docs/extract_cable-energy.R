# working directory
if (basename(getwd()) == 'nrel-cables') setwd('docs')

# load packages and variables ----
source('./packages_vars.R')

# cables: read ----
cbl2_sf = read_sf(dx2_geo)
cbl2_sp = cbl2_sf %>% as('Spatial')
cbl3_sf = read_sf(dx3_geo)
cbl3_sp = cbl3_sf %>% as('Spatial')

# usa_rgn
usa_rgn_s = read_sf(usa_rgn_s_geo)
usa_rgn = read_sf(usa_rgn_geo)

# depth classes
tbl_depth_reclass = tribble(
  ~depth_from, ~depth_to, ~depth_mid,
  -10000,         0,     -5000,
  0,       100,        50,
  100,       200,       150,
  200,      1000,       600,
  1000,     10000,      5000)
depth_levels = c(-5000,      50,       150,         600,     5000)
depth_labels = c( '<0', '0-100', '100-200', '200-1,000', '>1,000')

# tide ----
tide_breaks = c(0,500,1000,1500,10753)
tide_labels = c('0-500','500-1,000','1,000-1,500','>1,500')

if (!file.exists(tide_cbls_csv) | redo){ # TODO / DEBUG: remove | T
  
  # projection of original tide data: `st_crs(tide_east)$proj4string`
  crs_tide = '+proj=longlat +datum=NAD83 +no_defs'
  
  # tide: read data ----
  # NOTE: region (rgn) refers to analytical energy input area, 
  #       whereas territory (ter) is US territory from EEZ subdivisions
  #redo = T
  if (!file.exists(tide_csv) | redo | T){  # TODO / DEBUG: remove | T
    
    process_tide_pts = function(rgn){ # rgn='East' # rgn='West'
      
      cat(sprintf('rgn %s -- %s\n', rgn, Sys.time()))
      csv_rgn_pts = sprintf('../data/tide_rgn-%s_pts.csv', rgn)
      geo_rgn_pts = sprintf('../data/tide_rgn-%s_pts.geojson', rgn)
      
      if (!file.exists(csv_rgn_pts) | redo){
        
        # read in points, project to wrap around dateline
        pts = read_sf(tide_shps[[rgn]]) %>%
          as('Spatial') %>% 
          spTransform(crs_gcs_w) %>% 
          st_as_sf() %>%
          select(lon=LONGIT, lat=LATITU, pwr_wm2=MEANPO)
      
        # get bounding box for limiting USA territories
        cat(sprintf('  bbox -- %s\n', Sys.time()))
        b = st_bbox(pts)
        bb = st_polygon(list(matrix(c(
          b['xmin'], b['ymin'],
          b['xmin'], b['ymax'],
          b['xmax'], b['ymax'],
          b['xmax'], b['ymin'],
          b['xmin'], b['ymin']), ncol=2, byrow=T)))
  
        # limit simplified US territories to those intersecting pts bounding box
        usa_pts = usa_rgn_s %>%
          filter(st_intersects(geometry, bb, sparse=F)) %>%
          select(territory)
  
        # spatial join on simplified US territories: intersect and join territory column
        cat(sprintf('  st_join simple -- %s\n', Sys.time()))
        pts = st_join(pts, usa_pts, prepared=T) # 2.1 min for East
        
        # get points without a territory assigned
        pts_na = pts %>%
          filter(is.na(territory)) %>%
          select(-territory)
      
        # spatial join on high res US territories for pts without territory
        cat(sprintf('  st_join hi-res -- %s\n', Sys.time()))
        usa_pts = usa_rgn %>%
          filter(territory %in% unique(usa_pts$territory)) %>%
          select(territory)
        pts_na = st_join(pts_na, usa_pts, prepared=T) # 2 min on 87,025 East NAs
      
        # combine pts and pts_na
        cat(sprintf('  rbind -- %s\n', Sys.time()))
        pts = rbind(
          pts %>%
            filter(!is.na(territory)),
          pts_na) %>%
          mutate(
            region=rgn)
        
        # write to csv
        pts %>% 
          as_tibble() %>%
          write_csv(csv_rgn_pts)
        write_sf(pts, geo_rgn_pts, delete_dsn=T)
        
      } else {
        
        pts = read_csv(csv_rgn_pts) %>%
          mutate(
            longitude = ifelse(lon < 0, lon + 180, lon),
            latitude  = lat) %>%
          st_as_sf(coords = c('longitude','latitude'), agr='constant', crs=crs_gcs_w)
        
      } # end if (!file.exists(csv_rgn_pts)...
      
      # rasterize by territory
      pts = pts %>%
        filter(!is.na(territory))
      for (ter in unique(pts$territory)){ # ter = unique(pts$territory)[1] # ter = 'Alaska'
      #for (ter in c("Gulf of Mexico","Puerto Rico","US Virgin Islands")){ # TODO/DEBUG: rm line

        cat(sprintf('  ter %s -- %s\n', ter, Sys.time()))
        ter_s   = str_replace_all(ter,' ','-')
        tif     = sprintf('../data/tide_ter-%s.tif', ter_s)
        csv_r   = sprintf('../data/tide_ter-%s_tif.csv', ter_s)
        csv_c   = sprintf('../data/tide_ter-%s_tif-cable.csv', ter_s)
        csv_d   = sprintf('../data/tide_ter-%s_tif-depth.csv', ter_s)
        #csv_d_c = sprintf('../data/tide_ter-%s_tif-depth-cable.csv', ter_s)
        geo     = sprintf('../data/tide_ter-%s_pts.geojson', ter)
      
        p = pts %>%
          filter(territory==ter) %>% 
          as('Spatial')
        #write_sf(pts %>% filter(territory==ter), geo, delete_dsn=T)
        
        # rasterize
        if (!file.exists(tif) | redo){
          cat(sprintf('    rasterize -- %s\n', Sys.time()))
          r = rasterize(
            p,
            raster(crs=crs_gcs_w, ext=extent(p), resolution=tide_res_dd),
            field='pwr_wm2', fun=mean, na.rm=T) # 9.6 sec for East 0.005 res
          
          # write raster
          writeRaster(r, tif, overwrite=T)
        } else {
          r = raster(tif)
        }
        # stack with area
        s = stack(r, area(r))
        names(s) = c('pwr_wm2', 'area_km2') # plot(r)
        
        # get values for whole raster
        if (!file.exists(csv_r) | redo){
          cat(sprintf('    getValues -- %s\n', Sys.time()))
          v_r = getValues(s) %>% as_tibble() %>%
            filter(!is.na(pwr_wm2)) %>%
            mutate(
              territory = ter)
          write_csv(v_r, csv_r)
        } else {
          v_r = read_csv(csv_r)
        }
        
        # depth aligned to r
        depth_ter_grd = sprintf('../data/depth_%s.grd', str_replace(ter, ' ', '_'))
        depth_ter = raster(depth_ter_grd)
        depth_ter = resample(depth_ter, r, method='ngb') %>%
          crop(r) %>%
          mask(r)
        names(depth_ter) = 'depth' # plot(depth_ter)
        
        # cross-tabulate depth with tide energy
        if (!file.exists(csv_d) | T){ # TODO / DEBUG: off  | T

          cell_area_km2 = cellStats(s[['area_km2']],'mean')

          r_r = raster::cut(r, breaks=tide_breaks)
          names(r_r) = 'energy'
          # plot(r_r)
          
          x = crosstab(depth_ter, r_r)
          names(x) = c('depth','energy','n')
          
          tbl_tide_depth = x %>% # error for East: object 'depth' not found
            as_tibble() %>%
            mutate(
              territory = ter,
              depth_factor = factor(
                depth %>% as.numeric(),
                levels=depth_levels,
                labels=depth_labels),
              energy_factor = factor(
                energy %>% as.numeric(),
                levels=1:length(tide_labels),
                labels=tide_labels),
              area_km2 = n * cell_area_km2) %>%
            select(territory, depth_factor, energy_factor, area_km2) %>%
            filter(
              !is.na(depth_factor),
              !is.na(energy_factor),
              area_km2 > 0) # View(tbl_tide_depth)
          write_csv(tbl_tide_depth, csv_d)
        }

        s = stack(s, depth_ter)
        names(s)[length(names(s))] = 'depth'
        
        # extract values of cable overlapping raster
        cat(sprintf('    extract -- %s\n', Sys.time()))
        v_c = bind_rows(
          extract(s, 
                  cbl2_sp %>% subset(territory==ter), df=T) %>%
            mutate(
              cable = 'cable2'),
          extract(s, 
                  cbl3_sp %>% subset(territory==ter), df=T) %>%
            mutate(
              cable = 'cable3')) %>%
          as_tibble() %>%
          filter(!is.na(pwr_wm2)) %>%
          mutate(
            territory = ter)
        cat(sprintf('    write csv_c -- %s\n', Sys.time()))
        write_csv(v_c, csv_c) # v_c0 = read_csv(csv_c)
      } # end for (ter in...)
      
    } # end process_tide_pts = function()
    
    for (rgn in names(tide_shps)){ # rgn='West' # 20 min
      process_tide_pts(rgn)  
    }
    
  } # end if (!file.exists(tide_tif))

  # tide: summarize by territory, depth & energy

  # summarize for all energy
  f_v = list.files('../data', 'tide_ter-.*_tif-depth\\.csv$', full.names=T)
  d_v = bind_rows(lapply(f_v, read_csv))
  d_v = d_v %>%
     mutate(
       energy = factor(
         x      = energy_factor,
         levels = tide_labels,
         labels = tide_labels,
         ordered=T),
       depth = factor(
         x      = depth_factor,
         levels = depth_labels,
         labels = depth_labels,
         ordered=T)) %>%
    select(territory, depth, energy, area_km2) %>%
    arrange(territory, depth, energy)
  #View(d_v)
  #   group_by(territory, depth, energy_cat) %>%
  #   summarize(
  #     area_km2 = sum(area_km2))
  
  # summarize for overlap with cables
  f_c = list.files('../data', 'tide_ter-.*_tif-cable\\.csv$', full.names=T)
  d_c = bind_rows(lapply(f_c, read_csv)) %>%
    mutate(
      depth = factor(
        x      = depth,
        levels = depth_levels,
        labels = depth_labels,
        ordered=T),
      energy = cut(
        x      = pwr_wm2, 
        breaks = tide_breaks, 
        labels = tide_labels, 
        ordered_result = T, include.lowest=T)) %>%
    group_by(territory, depth, energy, cable) %>%
    summarize(
      area_km2 = sum(area_km2)) %>%
    ungroup() %>%
    spread(cable, area_km2) %>%
    rename(
      cable2_km2 = cable2,
      cable3_km2 = cable3)
  
  # tide: summmarize ----
  tide_cbls = d_v %>%
    left_join(
      d_c, 
      by=c('territory','depth','energy')) %>%
    replace_na(list(
      cable2_km2 = 0,
      cable3_km2 = 0)) %>%
    mutate(
      cable2_pct = cable2_km2 / area_km2,
      cable3_pct = cable3_km2 / area_km2) %>%
    select(
      territory, depth, energy, area_km2,
      cable2_km2, cable2_pct, cable3_km2, cable3_pct) %>%
    arrange(territory, depth, energy)
  
  # rbind territory=ALL
  tide_cbls = tide_cbls %>%
    bind_rows(
      tide_cbls %>%
        group_by(depth, energy) %>%
        summarize(
          area_km2   = sum(area_km2),
          cable2_km2 = sum(cable2_km2),
          cable3_km2 = sum(cable3_km2),
          territory = 'ALL') %>%
        mutate(
          cable2_pct     = cable2_km2 / area_km2,
          cable3_pct     = cable3_km2 / area_km2) %>%
        ungroup()) 
  #View(tide_cbls)
  
  # write to csv
  tide_cbls %>% write_csv(tide_cbls_depth_csv)
}

# wave: read ----
if (!file.exists(wave_geo)){
  wave_sf = read_sf(wave_shp) # 42234 features
  
  # wave: categorize and aggregate by energy classes ----
  wave_sf = wave_sf %>%
    filter(ann_wef != -9999) %>%
    select(lon, lat, ann_wef) %>%
    mutate(
      energy_cat = cut(ann_wef, c(0,10,20,30,52), include.lowest=T),
      energy_lbl = factor(energy_cat, levels(energy_cat), labels=c(sprintf('%d-%d', c(0,10,20), c(10,20,30)), '>30')),
      energy_num = factor(energy_cat, levels(energy_cat), labels=c(0,10,20,30)) %>% as.character() %>% as.integer())

  # transform lon from [-180,180] to [0,360]
  # [sf::st_transform not honoring +lon_wrap](https://github.com/edzer/sfr/issues/280)
  wave_sf = wave_sf %>%
    mutate(geometry = (geometry + c(360,90)) %% c(360) - c(0,90)) %>% 
    st_set_crs(crs_gcs_w)
  
  # join with territory
  wave_sf = st_join(wave_sf, usa_rgn_s %>% select(territory))
  
  # aggregate on territory energy breaks
  wave_sum_sf = wave_sf %>%
    group_by(territory, energy_lbl, energy_num) %>%
    summarise() %>% ungroup() %>%
    st_union()
    mutate(
      area_km2 = st_area(geometry) / (1000*1000))
      
  # write to geojson
  write_sf(wave_sum_sf, wave_geo, delete_dsn=T)
}
wave = read_sf(wave_geo)

# make valid geometry
if (any(!st_is_valid(wave))) {
  wave = wave %>%
    mutate(geometry = st_buffer(geometry, dist=0))
}

# wave: intersect with cables & dissolve on region & energy -----
if (any(!file.exists(wave_cbl2_geo), !file.exists(wave_cbl3_geo), redo)){
  
  wave_cbl2 = st_intersection(wave, cbl2_sf %>% select(-territory)) %>%
    group_by(territory, energy_lbl, energy_num) %>%
    summarise() %>% ungroup() %>%
    mutate(
      area_km2 = st_area(geometry) / (1000*1000)) %>%
    arrange(territory, energy_num)
  
  wave_cbl3 = st_intersection(wave, cbl3_sf %>% select(-territory)) %>%
    group_by(territory, energy_lbl, energy_num) %>%
    summarise() %>% ungroup() %>%
    mutate(
      area_km2 = st_area(geometry) / (1000*1000)) %>%
    arrange(territory, energy_num)
  
  # write to geojson
  wave_cbl2 %>% write_sf(wave_cbl2_geo, delete_dsn=T)
  wave_cbl3 %>% write_sf(wave_cbl3_geo, delete_dsn=T)
}
wave_cbl2 = read_sf(wave_cbl2_geo)
wave_cbl3 = read_sf(wave_cbl3_geo)

# wave: summarize by region & energy with cables into table -----
if (!file.exists(wave_cbls_csv)){
  
  wave_cbls = wave %>% as_tibble() %>% select(-geometry) %>%
    left_join(
      wave_cbl2 %>% as_tibble() %>% 
        select(-geometry, cable2_km2=area_km2),
      by=c('territory','energy_lbl','energy_num')) %>%
    left_join(
      wave_cbl3 %>% as_tibble() %>% 
        select(-geometry, cable3_km2=area_km2),
      by=c('territory','energy_lbl','energy_num')) %>%
    replace_na(list(
      cable2_km2 = 0,
      cable3_km2 = 0)) %>%
    mutate(
      cable2_pct     = cable2_km2 / area_km2,
      cable3_pct     = cable3_km2 / area_km2) %>%
    select(
      territory,
      energy_num, energy_lbl, area_km2,
      cable2_km2, cable2_pct, cable3_km2, cable3_pct) %>%
    arrange(territory, energy_num) # View(wave_cbls)
  
  # quick fix: remove *pct_all columns
  # read_csv(wave_cbls_csv) %>%
  #   select(-pct_all, -cable2_pct_all, -cable3_pct_all) %>%
  #   write_csv(wave_cbls_csv)
  
  # rbind territory=ALL
  wave_cbls = wave_cbls %>%
    bind_rows(
      wave_cbls %>%
        group_by(energy_num, energy_lbl) %>%
        summarize(
          area_km2   = sum(area_km2),
          cable2_km2 = sum(cable2_km2),
          cable3_km2 = sum(cable3_km2),
          territory = 'ALL') %>%
        mutate(
          cable2_pct     = cable2_km2 / area_km2,
          cable3_pct     = cable3_km2 / area_km2) %>%
        ungroup()) 
  
  wave_cbls %>% write_csv(wave_cbls_csv)
}

# wind: read ----
if (!file.exists(wind_geo)){
  
  for (i in 1:length(wind_shps)){ # i=1
    # setup
    shp = wind_shps[[i]]
    lbl = names(wind_shps)[i]
    cat(sprintf('%s - %s\n', lbl, shp))
    
    # read in vector
    v = read_sf(shp) %>%
      mutate(region = lbl) %>%
      as('Spatial')
    
    # clean geometry
    if (!gIsValid(v)) v = clgeo_Clean(v, strategy = 'BUFFER')
    
    # merge
    if (i == 1){
      wind = v
    } else {
      wind = raster::union(wind, v)
    }
  } # 1.3 min
  
  # wind: categorize and aggregate by energy classes ----
  wind@data = wind@data %>%
    mutate(
      energy_cat = cut(Speed_90, c(0,7:12)),
      energy_lbl = factor(energy_cat, levels(energy_cat), labels=c('<=7', sprintf('%d-%d', 7:11, 8:12))),
      energy_num = factor(energy_cat, levels(energy_cat), labels=7:12) %>% as.character() %>% as.integer())
  
  # dissolve on region & energy
  
  raster::intersect(wind, usa_rgn_s)
  
  wind = raster::aggregate(wind, by=c('region','energy_lbl','energy_num'))
  
  # calculate area
  wind$area_km2 = areaPolygon(wind) / (1000*1000)
  # wind@data %>% %>% summary # check for NAs
  
  # write to geojson
  wind %>% st_as_sf() %>% write_sf(wind_geo)
  
  # read wind, transform from [-180,180] to [0,360]
  wind = read_sf(wind_geo) %>%
    mutate(geometry = (geometry + c(360,90)) %% c(360) - c(0,90)) %>% 
    st_set_crs(crs_gcs_w)
  
  # intersect with regions, update area
  wind = st_join(wind, usa_rgn_s %>% select(territory)) %>%
    mutate(
      area_km2 = st_area(geometry) / (1000*1000))
  
  # write to geojson
  write_sf(wind, wind_geo, delete_dsn=T)
}
wind = read_sf(wind_geo)
if (any(!st_is_valid(wind))) wind = st_buffer(wind, 0)

# wind: intersect with cables & dissolve on region & energy -----
if (any(!file.exists(wind_cbl2_geo), !file.exists(wind_cbl3_geo), redo)){
  
  wind_cbl2 = st_intersection(wind, cbl2_sf %>% select(-territory)) %>%
    group_by(territory, energy_lbl, energy_num) %>%
    summarise() %>% ungroup() %>%
    mutate(
      area_km2 = st_area(geometry) / (1000*1000)) %>%
    arrange(territory, energy_num)
  
  wind_cbl3 = st_intersection(wind, cbl3_sf %>% select(-territory)) %>%
    group_by(territory, energy_lbl, energy_num) %>%
    summarise() %>% ungroup() %>%
    mutate(
      area_km2 = st_area(geometry) / (1000*1000)) %>%
    arrange(territory, energy_num)
  
  # write to geojson
  wind_cbl2 %>% write_sf(wind_cbl2_geo, delete_dsn=T)
  wind_cbl3 %>% write_sf(wind_cbl3_geo, delete_dsn=T)
}
wind_cbl2 = read_sf(wind_cbl2_geo)
wind_cbl3 = read_sf(wind_cbl3_geo)

# wind: summarize by region & energy with cables into table -----
if (!file.exists(wind_cbls_csv)){
  
  wind_cbls = wind %>% as_tibble() %>% select(-geometry) %>%
    left_join(
      wind_cbl2 %>% as_tibble() %>% 
        select(-geometry, cable2_km2=area_km2),
      by=c('territory','energy_lbl','energy_num')) %>%
    left_join(
      wind_cbl3 %>% as_tibble() %>% 
        select(-geometry, cable3_km2=area_km2),
      by=c('territory','energy_lbl','energy_num')) %>%
    replace_na(list(
      cable2_km2 = 0,
      cable3_km2 = 0)) %>%
    mutate(
      cable2_pct     = cable2_km2 / area_km2,
      cable3_pct     = cable3_km2 / area_km2) %>%
    select(
      territory,
      energy_num, energy_lbl, area_km2,
      cable2_km2, cable2_pct, cable3_km2, cable3_pct) %>%
    arrange(territory, energy_num) # View(wind_cbls)
  
  # rbind territory=ALL
  wind_cbls = wind_cbls %>%
    bind_rows(
      wind_cbls %>%
        group_by(energy_num, energy_lbl) %>%
        summarize(
          area_km2   = sum(area_km2),
          cable2_km2 = sum(cable2_km2),
          cable3_km2 = sum(cable3_km2),
          territory = 'ALL') %>%
        mutate(
          cable2_pct     = cable2_km2 / area_km2,
          cable3_pct     = cable3_km2 / area_km2) %>%
        ungroup()) 
  
  wind_cbls %>% write_csv(wind_cbls_csv)
}

# depth re-analysis, starting with study area depth rotate & crop ----
if (!file.exists(depth_grd)){
  
  # read in raster and rotate to [0,360]
  depth = raster(depth_nc, layer = 'elevation')
  depth_w = shift(rotate(shift(depth, 180)), 180) # wrap rotate from [0,360] to [-180,180]
  #plot(depth_w)
   
  # helper for tide bbox in [0, 180]
  # # tide_west
  # b = c(xmin=-179.65306, ymin=32.39362, xmax=-117.09722, ymax=61.48098)
  # # tide_east
  # b = c(xmin=-97.71923, ymin=17.55710, xmax=-64.43206, ymax=45.17222)
  # bb = st_polygon(list(matrix(c(
  #   b['xmin'], b['ymin'],
  #   b['xmin'], b['ymax'],
  #   b['xmax'], b['ymax'],
  #   b['xmax'], b['ymin'],
  #   b['xmin'], b['ymin']), ncol=2, byrow=T)))
  # bb_sf = st_cast(st_sf(a = 1, geometry=st_sfc(bb))) %>%
  #   st_set_crs(crs_gcs)
  # st_bbox(bb_sf)
  # mapview(bb_sf)
  # bb_sf = bb_sf  %>%
  #   mutate(geometry = (geometry + c(360,90)) %% c(360) - c(0,90)) %>% 
  #   st_set_crs(crs_gcs_w)
  # st_bbox(bb_sf)
  # mapview(bb_sf)
  
  # get extents of all input data of interest to get outer bbox
  #                                            # xmin ymin xmax ymax 
  # st_bbox(wave)                              #  171   16  297   64 
  # st_bbox(wind)                              #  199   18  294   50
  # read_sf(tide_shps[['West']]) %>% st_bbox() #  180   32  243   62
  # read_sf(tide_shps[['East']]) %>% st_bbox() #  262   17  296   46
  # bbox(cbl3_buf1px)                          #  141    4  296   62
  #                               # outer bbox #  141    4  297   64 
  
  depth_wc = crop(depth_w, extent(141, 297, 4, 64))
  #plot(depth_wc)

  writeRaster(depth_wc, depth_grd, overwrite=T)
} else {
  depth_wc = raster(depth_grd)
}

# depth per region for extraction of energy resource ---- 
if (F){
  for (i in seq_along(unique(usa_rgn_s$territory))){ # i=1
    ter = sort(unique(usa_rgn_s$territory))[i]
    cat(sprintf('%02d: %s -- %s\n', i, ter, Sys.time()))
  
    ter_extent = usa_rgn_s %>%
      filter(territory == ter) %>%
      as('Spatial') %>%
      extent()
    
    depth_ter = crop(depth_wc, ter_extent)
    depth_ter = depth_ter * -1
    depth_ter = reclassify(depth_ter, as.matrix(tbl_depth_reclass)) 
    
    plot(depth_ter)
    
    depth_ter_grd = sprintf('../data/depth_%s.grd', str_replace(ter, ' ', '_'))
    writeRaster(depth_ter, depth_ter_grd, overwrite=T)
  }
}

# prep depth with cbl3 mask ----
if (!file.exists(depth_m_cbl3_grd)){
  # resume...
  depth_wc = raster(depth_grd)
  
  # buffer out 1 pixel in decimal degrees
  cbl3_buf = st_buffer(cbl3_sf, dist=0.01) %>% as('Spatial')
  #plot(cbl3_buf1px)
  
  # rasterize buffer poly to mask depth
  system.time({
    cbl3_r = rasterize(cbl3_buf, depth_wc, 1)
  }) # 1.8 min
  #plot(cbl3_r)
  
  # mask out cells besides on/near cable lines
  system.time({
    #depth_wcc = crop(depth_wc, cbl3_r)
    depth_m_cbl3 = mask(depth_wc, cbl3_r)
  }) # 0.8 min
  
  # convert to bathymetric depth
  system.time({
    depth_m_cbl3[depth_m_cbl3 > 10] = NA
    depth_m_cbl3 = depth_m_cbl3 %>% trim()
    depth_m_cbl3 = depth_m_cbl3 * -1
  }) # 1 min
  # plot(depth_m_cbl3)
  #mapview(depth_m_cbl3)
  # leaflet() %>%
  #   
  #   addRasterImage(depth_m_cbl3)
  
  system.time({
    writeRaster(depth_m_cbl3, depth_m_cbl3_grd, overwrite=T)  # 100.5/60 min
  })
} else {
  depth_m_cbl3 = raster(depth_m_cbl3_grd) # plot(depth_m_cbl3)
}

# intersect cable buffers with reclassified depth raster ----- 
if (!file.exists(dx2_depth_geo) | !file.exists(dx2_depth_geo)){
  
  # create depth reclass table
  d_max = cellStats(depth_m_cbl3, 'max') # 7511
  
  # reclassify raster
  system.time({
    r_d3bin = reclassify(depth_m_cbl3, as.matrix(tbl_depth_reclass)) 
  }) # 17 sec
  #plot(r_d3bin)
  # leaflet() %>%
  #   addTiles() %>%
  #   addRasterImage(r_d3bin)
  #freq(r_d3bin)
  
  writeRaster(r_d3bin, '../data/r_d3bin.grd', overwrite=T)  # 206.1/60 min

  # convert raster to vector
  # iterate over regions so finishes and memory doesn't page to filesys vs:
  #   p_d3bin = rasterToPolygons(r_d3bin, dissolve=T)
  p_d3bin_ters = list()
  for (ter in sort(usa_rgn_s$territory)){ # ter = 'Guam'
    cat(sprintf('ter: %s\n', ter))

    ter_extent = usa_rgn_s %>%
      filter(territory == ter) %>%
      as('Spatial') %>%
      extent()
    
    r_d3bin_ter = crop(r_d3bin, ter_extent)
    
    p_d3bin_ters[[ter]] = rasterToPolygons(r_d3bin_ter, dissolve=T) %>% 
      ms_simplify(keep=0.3) %>%
      st_as_sf() %>%
      select(
        depth_bin=layer) %>%
      mutate(
        territory=ter)
  }
  p_d3bin = do.call('rbind', p_d3bin_ters)
  p_d3bin = p_d3bin %>%
    mutate(
      depth_bin = ifelse(depth_bin < 0, -5000, depth_bin),
      depth_factor = factor(
        x      = depth_bin, 
        levels = depth_levels, 
        labels = depth_labels,
        ordered=T))
  
  # fix if bad intersections
  if (any(!st_is_valid(p_d3bin))) {
    p_d3bin = p_d3bin %>%
      mutate(geometry = st_buffer(geometry, dist=0))
  }
  
  p_d3bin = p_d3bin %>%
    group_by(territory, depth_factor) %>%
    summarize() %>%
    ungroup()
  #table(p_d3bin$depth_factor)
  
  # pal = colorFactor('YlOrRd', p_d3bin$depth_factor, reverse=T)
  # leaflet(p_d3bin) %>%
  #   addProviderTiles('Esri.OceanBasemap', group = 'Ocean') %>%
  #   addProviderTiles('Stamen.TonerLite', group = 'B&W') %>%
  #   #addRasterImage(r_d3bin_ter) %>%
  #   addPolygons(
  #     group='p_d3bin',
  #     color=NA, fillColor = ~pal(depth_factor), fillOpacity = 0.5) %>%
  #   addPolygons(
  #     data=cbl3_sf, group='cbl3_sf',
  #     color=NA, fillColor ='black', fillOpacity = 0.5) %>%
  #   addLegend(
  #     position='bottomright', pal=pal, values=~depth_factor, title='depth_bin') %>%
  #   addLayersControl(
  #     baseGroups = c('Ocean','B&W'),
  #     overlayGroups = c('p_d3bin', 'cbl3_sf'),
  #     options = layersControlOptions(collapsed=F))

  write_sf(p_d3bin, '../data/p_d3bin.geojson', delete_dsn=T)
  
  # intersect buffers to depth vector
  system.time({
    cbl2_depth_sp = raster::intersect(
      cbl2_sp, 
      p_d3bin %>% 
        select(-territory) %>% 
        as('Spatial')) # 274.8/60 min
    cbl3_depth_sp = raster::intersect(
      cbl3_sp, 
      p_d3bin %>% 
        select(-territory) %>% 
        as('Spatial')) # 274.8/60 min
  }) # 10 sec
  
  # convert to simple feature
  cbl2_depth_sf = cbl2_depth_sp %>% st_as_sf()
  cbl3_depth_sf = cbl3_depth_sp %>% st_as_sf()
  
  # fix if bad intersections
  if (any(!st_is_valid(cbl2_depth_sf))) {
    cbl2_depth_sf = cbl2_depth_sf %>%
      mutate(geometry = st_buffer(geometry, dist=0))
  }
  if (any(!st_is_valid(cbl3_depth_sf))) {
    cbl3_depth_sf = cbl3_depth_sf %>%
      mutate(geometry = st_buffer(geometry, dist=0))
  }
  
  # calculate area
  cbl2_depth_sf = cbl2_depth_sf %>%
    mutate(
      area_km2 = st_area(geometry) %>% set_units(km^2))
  cbl3_depth_sf = cbl3_depth_sf %>%
    mutate(
      area_km2 = st_area(geometry) %>% set_units(km^2))
  
  pal = colorFactor('YlOrRd', cbl3_depth_sf$depth_factor, reverse=T)
  leaflet(cbl3_depth_sf) %>%
    addProviderTiles('Esri.OceanBasemap', group = 'Ocean') %>%
    addProviderTiles('Stamen.TonerLite', group = 'B&W') %>%
    #addRasterImage(r_d3bin_ter) %>%
    addPolygons(
      group='cbl3_depth_sf',
      color=NA, fillColor = ~pal(depth_factor), fillOpacity = 0.5) %>%
    addPolygons(
      data=cbl3_sf, group='cbl3_sf',
      color=NA, fillColor ='black', fillOpacity = 0.5) %>%
    addLegend(
      position='bottomright', pal=pal, values=~depth_factor, title='depth_bin') %>%
    addLayersControl(
      baseGroups = c('Ocean','B&W'),
      overlayGroups = c('cbl3_depth_sf', 'cbl3_sf'),
      options = layersControlOptions(collapsed=F))
  
  # write to geojson
  cbl2_depth_sf %>% write_sf(dx2_depth_geo, delete_dsn=T)
  cbl3_depth_sf %>% write_sf(dx3_depth_geo, delete_dsn=T)
} else {
  cbl2_depth_sf = read_sf(dx2_depth_geo)
  cbl3_depth_sf = read_sf(dx3_depth_geo)
}

# wave: intersect with depth-binned cables & dissolve on region & energy -----
wave_labels = c('0-10','10-20','20-30','>30')
if (any(!file.exists(wave_cbl2_depth_geo), !file.exists(wave_cbl3_depth_geo), redo)){
  
  # fix if bad intersections
  if (any(!st_is_valid(wave))) {
    wave = wave %>%
      mutate(geometry = st_buffer(geometry, dist=0))
  }

  wave = wave %>% 
    mutate(
      energy_factor = factor(
        x       = energy_lbl,
        labels  = wave_labels,
        ordered = T)) %>%
    select(-energy_num, -energy_lbl)
  
  wave_depth_cbl2 = st_intersection(
    wave %>% 
      select(-area_km2), 
    cbl2_depth_sf %>% 
      select(-territory, -area_km2)) %>%
    group_by(territory, depth_factor, energy_factor) %>%
    summarise() %>% 
    ungroup() %>%
    mutate(
      area_km2 = st_area(geometry) %>% set_units(km^2)) %>%
    arrange(territory, depth_factor, energy_factor)
  
  wave_depth_cbl3 = st_intersection(
    wave %>% 
      select(-area_km2), 
    cbl3_depth_sf %>% 
      select(-territory, -area_km2)) %>%
    group_by(territory, depth_factor, energy_factor) %>%
    summarise() %>% 
    ungroup() %>%
    mutate(
      area_km2 = st_area(geometry) %>% set_units(km^2)) %>%
    arrange(territory, depth_factor, energy_factor)
  
  # fix if bad intersections
  if (any(!st_is_valid(wave_depth_cbl2))) {
    wave_depth_cbl2 = wave_depth_cbl2 %>%
      mutate(geometry = st_buffer(geometry, dist=0))
  }
  if (any(!st_is_valid(wave_depth_cbl3))) {
    wave_depth_cbl3 = wave_depth_cbl3 %>%
      mutate(geometry = st_buffer(geometry, dist=0))
  }
  
  # write to geojson
  wave_depth_cbl2 %>% write_sf(wave_cbl2_depth_geo, delete_dsn=T)
  wave_depth_cbl3 %>% write_sf(wave_cbl3_depth_geo, delete_dsn=T)
}
wave_depth_cbl2 = read_sf(wave_cbl2_depth_geo) %>%
  mutate(
    depth_factor = factor(
      x      = depth_factor,
      levels = depth_labels,
      labels = depth_labels,
      ordered = T),
    energy_factor = factor(
      x      = energy_factor,
      levels = wave_labels,
      labels = wave_labels,
      ordered = T))
wave_depth_cbl3 = read_sf(wave_cbl3_depth_geo) %>%
  mutate(
    depth_factor = factor(
      x      = depth_factor,
      levels = depth_labels,
      labels = depth_labels,
      ordered = T),
    energy_factor = factor(
      x      = energy_factor,
      levels = wave_labels,
      labels = wave_labels,
      ordered = T))

# wave: extract depth with energy by region -----
if (!file.exists(wave_depth_csv)){
  lst_wave_depth = list()
  for (i in seq_along(na.omit(unique(wave$territory)))){ # i=5
    ter = sort(na.omit(unique(wave$territory)))[i]
    cat(sprintf('%02d: %s -- %s\n', i, ter, Sys.time()))
    
    depth_ter_grd = sprintf('../data/depth_%s.grd', str_replace(ter, ' ', '_'))
    depth_ter = raster(depth_ter_grd)
    #plot(depth_ter)
    
    system.time({
      wave_ter = rasterize(
        wave %>%
          filter(territory==ter) %>% 
          as('Spatial'), 
        depth_ter, 
        'energy_factor')
    }) # Puerto Rico: 6 sec; Alaska: 1:39 min:sec
    # plot(wave_ter)
    depth_ter = mask(depth_ter, wave_ter)
    cell_area_km2 = cellStats(area(wave_ter),'mean')
    
    lst_wave_depth[[ter]] = crosstab(depth_ter, wave_ter) %>%
      as_tibble() %>%
      mutate(
        territory = ter,
        depth_factor = factor(
          Var1 %>% as.numeric(), 
          levels=1:length(depth_labels), 
          labels=depth_labels),
        energy_factor = factor(
          Var2 %>% as.numeric(), 
          levels=1:length(wave_labels), 
          labels=wave_labels),
        area_km2 = Freq * cell_area_km2) %>%
      select(territory, depth_factor, energy_factor, area_km2) %>%
      filter(
        !is.na(depth_factor),
        !is.na(energy_factor),
        area_km2 > 0)
  }
  wave_depth = bind_rows(lst_wave_depth)
  write_csv(wave_depth, wave_depth_csv)
} # 2:42 min:sec
wave_depth = read_csv(wave_depth_csv) %>%
  mutate(
    depth_factor = factor(
      x      = depth_factor,
      levels = depth_labels,
      labels = depth_labels,
      ordered = T),
    energy_factor = factor(
      x      = energy_factor,
      levels = wave_labels,
      labels = wave_labels,
      ordered = T))

# wave: summarize depth & energy by region with cables into table -----
if (!file.exists(wave_depth_cbls_csv)){
  
  wave_depth_cbls = wave_depth %>% 
    left_join(
      wave_depth_cbl2 %>% 
        st_set_geometry(NULL) %>%
        rename(cable2_km2=area_km2),
      by=c('territory','depth_factor','energy_factor')) %>%
    left_join(
      wave_depth_cbl3 %>% 
        st_set_geometry(NULL) %>%
        rename(cable3_km2=area_km2),
      by=c('territory','depth_factor','energy_factor')) %>%
    replace_na(list(
      cable2_km2 = 0,
      cable3_km2 = 0)) %>%
    mutate(
      cable2_pct     = cable2_km2 / area_km2,
      cable3_pct     = cable3_km2 / area_km2) %>%
    select(
      territory,
      depth_factor, energy_factor, area_km2,
      cable2_km2, cable2_pct, cable3_km2, cable3_pct) %>%
    arrange(territory, depth_factor, energy_factor) # View(wave_cbls)
  
  # rbind territory=ALL
  wave_depth_cbls = wave_depth_cbls %>%
    bind_rows(
      wave_depth_cbls %>%
        group_by(depth_factor, energy_factor) %>%
        summarize(
          area_km2   = sum(area_km2),
          cable2_km2 = sum(cable2_km2),
          cable3_km2 = sum(cable3_km2),
          territory = 'ALL') %>%
        mutate(
          cable2_pct = cable2_km2 / area_km2,
          cable3_pct = cable3_km2 / area_km2) %>%
        ungroup()) # View(wave_depth_cbls)
  
  wave_depth_cbls %>% write_csv(wave_depth_cbls_csv)
}

# wind: intersect with depth-binned cables & dissolve on region & energy -----
wind_labels = c('<=7', '7-8', '8-9', '9-10', '10-11', '11-12')
if (any(!file.exists(wind_cbl2_depth_geo), !file.exists(wind_cbl3_depth_geo), redo)){
  
  # fix if bad intersections
  if (any(!st_is_valid(wind))) {
    wind = wind %>%
      mutate(geometry = st_buffer(geometry, dist=0))
  }
  
  wind = wind %>% 
    mutate(
      energy_factor = factor(
        x       = energy_lbl,
        labels  = wind_labels,
        ordered = T)) %>%
    select(-energy_num, -energy_lbl)
  
  wind_depth_cbl2 = st_intersection(
    wind %>% 
      select(-area_km2), 
    cbl2_depth_sf %>% 
      select(-territory, -area_km2)) %>%
    group_by(territory, depth_factor, energy_factor) %>%
    summarise() %>% 
    ungroup() %>%
    mutate(
      area_km2 = st_area(geometry) %>% set_units(km^2)) %>%
    arrange(territory, depth_factor, energy_factor)
  
  wind_depth_cbl3 = st_intersection(
    wind %>% 
      select(-area_km2), 
    cbl3_depth_sf %>% 
      select(-territory, -area_km2)) %>%
    group_by(territory, depth_factor, energy_factor) %>%
    summarise() %>% 
    ungroup() %>%
    mutate(
      area_km2 = st_area(geometry) %>% set_units(km^2)) %>%
    arrange(territory, depth_factor, energy_factor)
  
  # fix if bad intersections
  if (any(!st_is_valid(wind_depth_cbl2))) {
    wind_depth_cbl2 = wind_depth_cbl2 %>%
      mutate(geometry = st_buffer(geometry, dist=0))
  }
  if (any(!st_is_valid(wind_depth_cbl3))) {
    wind_depth_cbl3 = wind_depth_cbl3 %>%
      mutate(geometry = st_buffer(geometry, dist=0))
  }
  
  # write to geojson
  wind_depth_cbl2 %>% write_sf(wind_cbl2_depth_geo, delete_dsn=T)
  wind_depth_cbl3 %>% write_sf(wind_cbl3_depth_geo, delete_dsn=T)
}
wind_depth_cbl2 = read_sf(wind_cbl2_depth_geo) %>%
  mutate(
    depth_factor = factor(
      x      = depth_factor,
      levels = depth_labels,
      labels = depth_labels,
      ordered = T),
    energy_factor = factor(
      x      = energy_factor,
      levels = wind_labels,
      labels = wind_labels,
      ordered = T))
wind_depth_cbl3 = read_sf(wind_cbl3_depth_geo) %>%
  mutate(
    depth_factor = factor(
      x      = depth_factor,
      levels = depth_labels,
      labels = depth_labels,
      ordered = T),
    energy_factor = factor(
      x      = energy_factor,
      levels = wind_labels,
      labels = wind_labels,
      ordered = T))

# wind: extract depth with energy by region -----
if (!file.exists(wind_depth_csv)){
  lst_wind_depth = list()
  for (i in seq_along(na.omit(unique(wind$territory)))){ # i=5
    ter = sort(na.omit(unique(wind$territory)))[i]
    cat(sprintf('%02d: %s -- %s\n', i, ter, Sys.time()))
    
    depth_ter_grd = sprintf('../data/depth_%s.grd', str_replace(ter, ' ', '_'))
    depth_ter = raster(depth_ter_grd)
    #plot(depth_ter)
    
    system.time({
      wind_ter = rasterize(
        wind %>%
          filter(territory==ter) %>% 
          as('Spatial'), 
        depth_ter, 
        'energy_factor')
    }) # Puerto Rico: 6 sec; Alaska: 1:39 min:sec
    # plot(wind_ter)
    depth_ter = mask(depth_ter, wind_ter)
    cell_area_km2 = cellStats(area(wind_ter),'mean')
    
    lst_wind_depth[[ter]] = crosstab(depth_ter, wind_ter) %>%
      as_tibble() %>%
      mutate(
        territory = ter,
        depth_factor = factor(
          Var1 %>% as.numeric(), 
          levels=1:length(depth_labels), 
          labels=depth_labels),
        energy_factor = factor(
          Var2 %>% as.numeric(), 
          levels=1:length(wind_labels), 
          labels=wind_labels),
        area_km2 = Freq * cell_area_km2) %>%
      select(territory, depth_factor, energy_factor, area_km2) %>%
      filter(
        !is.na(depth_factor),
        !is.na(energy_factor),
        area_km2 > 0)
  }
  wind_depth = bind_rows(lst_wind_depth)
  write_csv(wind_depth, wind_depth_csv)
} # 1:11 min:sec
wind_depth = read_csv(wind_depth_csv) %>%
  mutate(
    depth_factor = factor(
      x      = depth_factor,
      levels = depth_labels,
      labels = depth_labels,
      ordered = T),
    energy_factor = factor(
      x      = energy_factor,
      levels = wind_labels,
      labels = wind_labels,
      ordered = T))

# wind: summarize depth & energy by region with cables into table -----
if (!file.exists(wind_depth_cbls_csv)){
  
  wind_depth_cbls = wind_depth %>% 
    left_join(
      wind_depth_cbl2 %>% 
        st_set_geometry(NULL) %>%
        rename(cable2_km2=area_km2),
      by=c('territory','depth_factor','energy_factor')) %>%
    left_join(
      wind_depth_cbl3 %>% 
        st_set_geometry(NULL) %>%
        rename(cable3_km2=area_km2),
      by=c('territory','depth_factor','energy_factor')) %>%
    replace_na(list(
      cable2_km2 = 0,
      cable3_km2 = 0)) %>%
    mutate(
      cable2_pct     = cable2_km2 / area_km2,
      cable3_pct     = cable3_km2 / area_km2) %>%
    select(
      territory,
      depth_factor, energy_factor, area_km2,
      cable2_km2, cable2_pct, cable3_km2, cable3_pct) %>%
    arrange(territory, depth_factor, energy_factor) # View(wind_cbls)
  
  # rbind territory=ALL
  wind_depth_cbls = wind_depth_cbls %>%
    bind_rows(
      wind_depth_cbls %>%
        group_by(depth_factor, energy_factor) %>%
        summarize(
          area_km2   = sum(area_km2),
          cable2_km2 = sum(cable2_km2),
          cable3_km2 = sum(cable3_km2),
          territory = 'ALL') %>%
        mutate(
          cable2_pct = cable2_km2 / area_km2,
          cable3_pct = cable3_km2 / area_km2) %>%
        ungroup()) # View(wind_depth_cbls)
  
  wind_depth_cbls %>% write_csv(wind_depth_cbls_csv)
}

