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

# tide ----
if (!file.exists(tide_cbls_csv) | redo){
  
  # projection of original tide data: `st_crs(tide_east)$proj4string`
  crs_tide = '+proj=longlat +datum=NAD83 +no_defs'
  
  # tide: read data ----
  # NOTE: region (rgn) refers to analytical energy input area, 
  #       whereas territory (ter) is US territory from EEZ subdivisions
  #redo = T
  if (!file.exists(tide_csv) | redo){ 
    
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

        cat(sprintf('  ter %s -- %s\n', ter, Sys.time()))
        ter_s = str_replace_all(ter,' ','-')
        tif   = sprintf('../data/tide_ter-%s.tif', ter_s)
        csv_r = sprintf('../data/tide_ter-%s_tif.csv', ter_s)
        csv_c = sprintf('../data/tide_ter-%s_tif-cable.csv', ter_s)
        geo   = sprintf('../data/tide_ter-%s_pts.geojson', ter)
      
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
        names(s) = c('pwr_wm2', 'area_km2')
        
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

  # tide: summarize table by territory, energy breaks
  brks = c(0,500,1000,1500,10753)
  
  # summarize for all raster values
  f_v = list.files('../data', 'tide_ter-.*_tif\\.csv$', full.names=T)
  d_v = bind_rows(lapply(f_v, read_csv)) %>%
    mutate(
      energy_cat = cut(pwr_wm2, brks, include.lowest=T)) %>%
    group_by(territory, energy_cat) %>%
    summarize(
      area_km2 = sum(area_km2))
  
  # summarize for overlap with cables
  f_c = list.files('../data', 'tide_ter-.*_tif-cable\\.csv$', full.names=T)
  d_c = bind_rows(lapply(f_c, read_csv)) %>%
    mutate(
      energy_cat = cut(pwr_wm2, brks, include.lowest=T)) %>%
    group_by(territory, energy_cat, cable) %>%
    summarize(
      area_km2 = sum(area_km2)) %>%
    ungroup() %>%
    spread(cable, area_km2) %>%
    rename(
      cable2_km2 = cable2,
      cable3_km2 = cable3)
  
  # tide: summmarize ----
  tide_cbls = d_v %>%
    left_join(d_c, by=c('territory','energy_cat')) %>%
    replace_na(list(
      cable2_km2 = 0,
      cable3_km2 = 0)) %>%
    mutate(
      cable2_pct     = cable2_km2 / area_km2 * 100,
      cable3_pct     = cable3_km2 / area_km2 * 100) %>%
    mutate(
      energy_lbl = factor(energy_cat, levels(energy_cat), labels=c(sprintf('%d-%d', brks[1:3], brks[2:4]), '>1500')),
      energy_num = factor(energy_cat, levels(energy_cat), labels=brks[1:4]) %>% as.character() %>% as.integer()) %>%
    select(
      territory, energy_num, energy_lbl, area_km2,
      cable2_km2, cable2_pct, cable3_km2, cable3_pct) %>%
    arrange(territory, energy_num)
  
  # rbind territory=ALL
  tide_cbls = tide_cbls %>%
    bind_rows(
      tide_cbls %>%
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
  
  # write to csv
  tide_cbls %>% write_csv(tide_cbls_csv)
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
