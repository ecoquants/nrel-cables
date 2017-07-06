# working directory
if (basename(getwd()) == 'nrel-cables') setwd('docs')

# load packages and variables ----
source('./packages_vars.R')

# cables: read ----
cbl2    = read_sf(dx2_geo) %>% as('Spatial')
cbl3    = read_sf(dx3_geo) %>% as('Spatial')
usa_rgn = read_sf(usa_rgn_geo) %>% as('Spatial')
suppressWarnings({
  proj4string(cbl2) = crs_gcs_w
  proj4string(cbl3) = crs_gcs_w
  proj4string(usa_rgn) = crs_gcs_w
})

# usa_rgn
usa_rgn_s = read_sf(usa_rgn_s_geo)
usa_rgn = read_sf(usa_rgn_geo)

# tide ----
if (!file.exists(tide_tif)){
  
  # projection of original tide data: `st_crs(tide_east)$proj4string`
  crs_tide = '+proj=longlat +datum=NAD83 +no_defs'
  
  # tide: read data ----
  # NOTE: region (rgn) refers to analytical energy input area, 
  #       whereas territory (ter) is US territory from EEZ subdivisions
  if (!file.exists(tide_csv)){
    
    process_tide_pts = function(rgn){
      
      cat(sprintf('rgn %s -- %s\n', rgn, Sys.time()))
      csv_rgn_pts = sprintf('../data/tide_rgn-%s_pts.csv', rgn)
      
      # read in points, project to wrap around dateline
      pts = read_sf(tide_shps[['East']]) %>%
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
          filter(is.na(territory)),
        pts_na) %>%
        mutate(
          region=rgn)
      
      # write to csv
      pts %>% 
        as_tibble() %>%
        write_csv(csv_rgn_pts)
      
      # rasterize by territory
      pts = pts %>%
        filter(!is.na(territory))
      for (ter in unique(pts$territory)){ # ter = unique(pts$territory)[1]

        cat(sprintf('  ter %s -- %s\n', ter, Sys.time()))
        ter_s   = str_replace(ter,' ','-')
        tif   = sprintf('../data/tide_ter-%s.tif', ter_s)
        csv_r = sprintf('../data/tide_ter-%s_tif.csv', ter_s)
        csv_c = sprintf('../data/tide_ter-%s_tif-cable.csv', ter_s)
        
        p = pts %>%
          filter(territory==ter) %>% 
          as('Spatial')
        
        # rasterize
        cat(sprintf('    rasterize -- %s\n', Sys.time()))
        r = rasterize(
          p,
          raster(crs=crs_gcs_w, ext=extent(p), resolution=tide_res_dd),
          field='pwr_wm2', fun=mean) # 9.6 sec for East 0.005 res

        # write raster
        writeRaster(r, tif, overwrite=T)
        
        # stack with area
        s = stack(r, area(r))
        names(s) = c('pwr_wm2', 'area_km2')
        
        cat(sprintf('    getValues -- %s\n', Sys.time()))
        v_r = getValues(s) %>% as_tibble() %>%
          filter(!is.na(pwr_wm2)) %>%
          mutate(
            territory = ter)
        write_csv(v_r, csv_r)
        
        cat(sprintf('    extract -- %s\n', Sys.time()))
        v_c = bind_rows(
          extract(s, 
                  cbl2 %>% subset(territory==ter), df=T) %>%
            mutate(
              cable = 'cable2'),
          extract(s, 
                  cbl3 %>% subset(territory==ter), df=T) %>%
            mutate(
              cable = 'cable3')) %>%
          as_tibble() %>%
          filter(!is.na(pwr_wm2)) %>%
          mutate(
            territory = ter)
        write_csv(v_r, csv_c)
      } # end for (ter in...)
      
    } # end process_tide_pts
    
    for (rgn in names(tide_shps)){
      process_tide_pts(rgn)  
    }
    
  }

  # TODO: in report.Rmd, leaflet viz tide rasters per territory w/ regex in data/tide_ter-*.tif
  # TODO: join data from territories for summarizing below
  
  # tide: summarize table by region, energy breaks
  brks = c(0,500,1000,1500,10753)
  
  d_r = bind_rows(rgn_v) %>%
    mutate(
      energy_cat = cut(pwr_wm2, brks, include.lowest=T)) %>%
    group_by(region, energy_cat) %>%
    summarize(
      area_km2 = sum(area_km2))
  
  d_c = bind_rows(rgn_c) %>%
    mutate(
      energy_cat = cut(pwr_wm2, brks, include.lowest=T)) %>%
    group_by(region, energy_cat, cable) %>%
    summarize(
      area_km2 = sum(area_km2)) %>%
    ungroup() %>%
    spread(cable, area_km2) %>%
    rename(
      cable2_km2 = cable2,
      cable3_km2 = cable3)
  
  # tide: summmarize ----
  tide_cbls = d_r %>%
    left_join(d_c, by=c('region','energy_cat')) %>%
    replace_na(list(
      cable2_km2 = 0,
      cable3_km2 = 0)) %>%
    mutate(
      cable2_pct     = cable2_km2 / area_km2 * 100,
      cable3_pct     = cable3_km2 / area_km2 * 100,
      pct_all        = area_km2 / sum(area_km2) * 100,
      cable2_pct_all = cable2_km2 / sum(area_km2) * 100,
      cable3_pct_all = cable3_km2 / sum(area_km2) * 100) %>%
    mutate(
      energy_lbl = factor(energy_cat, levels(energy_cat), labels=c(sprintf('%d-%d', brks[1:3], brks[2:4]), '>1500')),
      energy_num = factor(energy_cat, levels(energy_cat), labels=brks[1:4]) %>% as.character() %>% as.integer()) %>%
    select(
      region, energy_num, energy_lbl, area_km2, pct_all,
      cable2_km2, cable2_pct, cable2_pct_all, cable3_km2, cable3_pct, cable3_pct_all) %>%
    arrange(region, energy_num)
  
  tide_cbls %>% write_csv(tide_cbls_csv)
}

# wave: read ----
if (!file.exists(wave_geo)){
  wave = read_sf(wave_shp) # 42234 features
  
  # wave: categorize and aggregate by energy classes ----
  wave = wave %>%
    filter(ann_wef != -9999) %>%
    mutate(
      energy_cat = cut(ann_wef, c(0,10,20,30,52), include.lowest=T),
      energy_lbl = factor(energy_cat, levels(energy_cat), labels=c(sprintf('%d-%d', c(0,10,20), c(10,20,30)), '>30')),
      energy_num = factor(energy_cat, levels(energy_cat), labels=c(0,10,20,30)) %>% as.character() %>% as.integer()) %>%
    as('Spatial')
  
  # dissolve on energy class
  wave = raster::aggregate(wave, by=c('region','energy_lbl','energy_num'))
  
  # calculate area
  wave$area_km2 = areaPolygon(wave) / (1000*1000)
  
  # write to geojson
  wave %>% st_as_sf() %>% write_sf(wave_geo)
}
wave = read_sf(wave_geo) %>% as('Spatial')

# wave: intersect with cables & dissolve on region & energy -----
if (any(!file.exists(wave_cbl2_geo), !file.exists(wave_cbl3_geo))){
  wave_cbl2 = raster::intersect(wave, cbl2) %>%
    raster::aggregate(by=c('energy_lbl','energy_num'))
  wave_cbl3 = raster::intersect(wave, cbl3) %>%
    raster::aggregate(by=c('energy_lbl','energy_num'))
  
  # calculate area
  wave_cbl2$cable2_km2 = areaPolygon(wave_cbl2) / (1000*1000)
  wave_cbl3$cable3_km2 = areaPolygon(wave_cbl3) / (1000*1000)
  
  # write to geojson
  wave_cbl2 %>% st_as_sf() %>% write_sf(wave_cbl2_geo)
  wave_cbl3 %>% st_as_sf() %>% write_sf(wave_cbl3_geo)
}
wave_cbl2 = read_sf(wave_cbl2_geo) %>% as('Spatial')
wave_cbl3 = read_sf(wave_cbl3_geo) %>% as('Spatial')

# wave: summarize by region & energy with cables into table -----
if (!file.exists(wave_cbls_csv)){
  
  wave_cbls = wave@data %>%
    left_join(
      wave_cbl3@data,
      by=c('energy_lbl','energy_num')) %>%
    left_join(
      wave_cbl2@data,
      by=c('energy_lbl','energy_num')) %>%
    replace_na(list(
      cable2_km2 = 0,
      cable3_km2 = 0)) %>%
    mutate(
      cable2_pct     = cable2_km2 / area_km2 * 100,
      cable3_pct     = cable3_km2 / area_km2 * 100,
      pct_all        = area_km2 / sum(area_km2) * 100,
      cable2_pct_all = cable2_km2 / sum(area_km2) * 100,
      cable3_pct_all = cable3_km2 / sum(area_km2) * 100) %>%
    select(
      energy_num, energy_lbl, area_km2, pct_all,
      cable2_km2, cable2_pct, cable2_pct_all, cable3_km2, cable3_pct, cable3_pct_all) %>%
    arrange(energy_num)
  
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
}
wind = read_sf(wind_geo) %>% as('Spatial')

# wind: intersect with cables & dissolve on region & energy -----
if (any(!file.exists(wind_cbl2_geo), !file.exists(wind_cbl3_geo))){
  wind_cbl2 = raster::intersect(wind, cbl2) %>%
    raster::aggregate(by=c('region','energy_lbl','energy_num'))
  wind_cbl3 = raster::intersect(wind, cbl3) %>%
    raster::aggregate(by=c('region','energy_lbl','energy_num'))
  
  # calculate area
  wind_cbl2$cable2_km2 = areaPolygon(wind_cbl2) / (1000*1000)
  wind_cbl3$cable3_km2 = areaPolygon(wind_cbl3) / (1000*1000)
  
  # write to geojson
  wind_cbl2 %>% st_as_sf() %>% write_sf(wind_cbl2_geo)
  wind_cbl3 %>% st_as_sf() %>% write_sf(wind_cbl3_geo)
}
wind_cbl2 = read_sf(wind_cbl2_geo) %>% as('Spatial')
wind_cbl3 = read_sf(wind_cbl3_geo) %>% as('Spatial')

# wind: summarize by region & energy with cables into table -----
if (!file.exists(wind_cbls_csv)){
  
  wind_cbls = wind@data %>%
    left_join(
      wind_cbl3@data,
      by=c('region','energy_lbl','energy_num')) %>%
    left_join(
      wind_cbl2@data,
      by=c('region','energy_lbl','energy_num')) %>%
    replace_na(list(
      cable2_km2 = 0,
      cable3_km2 = 0)) %>%
    mutate(
      cable2_pct = cable2_km2 / area_km2 * 100,
      cable3_pct = cable3_km2 / area_km2 * 100) %>%
    group_by(region) %>%
    mutate(
      pct_region        = area_km2 / sum(area_km2) * 100,
      cable2_pct_region = cable2_km2 / sum(area_km2) * 100,
      cable3_pct_region = cable3_km2 / sum(area_km2) * 100) %>%
    ungroup() %>%
    select(
      region, energy_num, energy_lbl, area_km2, pct_region,
      cable2_km2, cable2_pct, cable2_pct_region, cable3_km2, cable3_pct, cable3_pct_region) %>%
    arrange(region, energy_num)
  
  wind_cbls %>% write_csv(wind_cbls_csv)
}
