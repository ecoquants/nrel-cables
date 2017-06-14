# packages: load, installing if needed ----
packages = c('tidyverse','rgdal','raster','geosphere', #'deldir',
             'rgeos','edzer/sfr','eblondel/cleangeo','geojsonio')
for (pkg in packages){ # pkg= packages[1] # pkg = 'edzer/sfr'
  github_pkg = grepl('/', pkg)
  p = ifelse(github_pkg, sub('(.*)/(.*)', '\\2', pkg), pkg)
  if (pkg == 'edzer/sfr') p = 'sf' # installed MacOS dependencies: https://github.com/edzer/sfr#macos
  if (!require(p, character.only=T)){
    if (github_pkg){
      if (!require(devtools)) install.packages('devtools')
      devtools::install_github(pkg)
    } else {
      install.packages(p)
    }
    library(p, character.only=T)
  }
}
# override functions with duplicate names in different packages
select = dplyr::select

# working directory
if (basename(getwd()) == 'nrel-cables') setwd('docs')

# variables & paths ----
d_incr = 100 # depth increment

# paths
dx2_geo       = sprintf('../data/buf_2xdepth-incr%sm.geojson', d_incr)
dx3_geo       = sprintf('../data/buf_3xdepth-incr%sm.geojson', d_incr)
# wind
wind_shps = list(
  'Atlantic'       = '/Volumes/Best HD/nrel_data_big/nrel.gov/wind/atl/atlantic_coast_90mwindspeed_off.shp',
  #gl  = '/Volumes/Best HD/nrel_data_big/nrel.gov/wind/gl/great_lakes_90mwindspeed_off.shp',
  'Gulf of Mexico' = '/Volumes/Best HD/nrel_data_big/nrel.gov/wind/gom/gulf_of_mexico_90mwindspeed_off.shp',
  'Hawaii'         = '/Volumes/Best HD/nrel_data_big/nrel.gov/wind/hi/HI_90mwindspeed_off.shp',
  'Pacific'        = '/Volumes/Best HD/nrel_data_big/nrel.gov/wind/pac/pacific_coast_90mwindspeed_off.shp')
wind_geo      = '../data/wind.geojson'
wind_cbl2_geo = '../data/wind_cable2.geojson'
wind_cbl3_geo = '../data/wind_cable3.geojson'
wind_cbls_csv = '../data/wind_cables.csv'
# wave
wave_shp      = '/Volumes/Best HD/nrel_data_big/nrel.gov/wave/mhk-atlas_wave_wef_ann/wave_wef_ann.shp'
wave_geo      = '../data/wave.geojson'
wave_cbl2_geo = '../data/wave_cable2.geojson'
wave_cbl3_geo = '../data/wave_cable3.geojson'
wave_cbls_csv = '../data/wave_cables.csv'
# tide
tide_shps    = list(
  West = '/Volumes/Best HD/nrel_data_big/nrel.gov/tide/tide_data_west/tide_data_west.shp',
  East = '/Volumes/Best HD/nrel_data_big/nrel.gov/tide/tide_data_east/tide_data_east.shp')
tide_tif      = '../data/tide.tif'
tide_csv      = '../data/tide.csv'
tide_cbls_csv = '../data/tide_cables.csv'

# cables: read ----
cbl2 = read_sf(dx2_geo) %>%
  select(-buffer) %>% 
  as('Spatial')
cbl3 = read_sf(dx3_geo) %>% 
  select(-buffer) %>% 
  as('Spatial')
cbl_prj = st_crs(cbl2 %>% st_as_sf())$proj4string

# tide ----
if (!file.exists(tide_geo)){
  
  # projection of original tide data: `st_crs(tide_east)$proj4string`
  tide_prj = '+proj=longlat +datum=NAD83 +no_defs'
  
  # tide: read data ----
  if (!file.exists(tide_csv)){
    bind_rows(
      read_sf(tide_shps[['East']]) %>% 
        as_tibble() %>%
        select(lon=LONGIT, lat=LATITU, pwr_wm2=MEANPO) %>%
        mutate(
          region = 'East'),
      read_sf(tide_shps[['West']]) %>% 
        as_tibble() %>%
        select(lon=LONGIT, lat=LATITU, pwr_wm2=MEANPO) %>%
        mutate(
          region = 'West')) %>%
      write_csv(tide_csv)
  }
  td = read_csv(tide_csv)
  # hist(td$pwr_wm2); max(td$pwr_wm2)
  
  # tide: convert to points
  tp = as.data.frame(td)
  row.names(tp) = tp$id
  coordinates(tp) = ~lon + lat
  proj4string(tp) = tide_prj
  tp = spTransform(tp, cbl_prj)
  
  # tide: rasterize and extract by region
  rgn_r = list(); rgn_v = list(); rgn_c = list()
  for (rgn in c('East','West')){ # rgn='East'
    
    pts = subset(tp, region==rgn)
    rgn_r[[rgn]] = rasterize(
      pts, 
      raster(crs=cbl_prj, ext=extent(pts), resolution=0.01), 
      field='pwr_wm2', fun=mean)

    s = stack(r, area(r))
    names(s) = c('pwr_wm2', 'area_km2')

    rgn_v[[rgn]] = getValues(s) %>% as_tibble() %>%
      filter(!is.na(pwr_wm2)) %>%
      mutate(
        region = rgn)

    rgn_c[[rgn]] = bind_rows(
      extract(s, cbl2, df=T) %>%
        mutate(
          cable = 'cable2'),
      extract(s, cbl3, df=T) %>%
        mutate(
          cable = 'cable3')) %>%
      as_tibble() %>%
      filter(!is.na(pwr_wm2)) %>%
      mutate(
        region = rgn)
  }
  
  # tide: merge rasters & save as tif
  r_all = merge(rgn_r[['East']], rgn_r[['West']], tolerance=0.2)
  writeRaster(r_all, tide_tif)

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
# units: wave energy flux (kW/m)
if (!file.exists(wave_geo)){
  wave = read_sf(wave_shp) # 42234 features
  
  # wave: categorize and aggregate by energy classes ----
  # TODO: rerun wave with cut(include.lowest=T) to get rid of NAs in wave_cables.csv
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
  # wave@data %>% %>% summary # check for NAs
  
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
# TODO: stacked histogram by cbl2/(cbl3-cbl2)/other; hist(wind$Speed_90)
#
# Musial et al (2016): technical potential 7 - 11.5 m/s
# - wind speed (m/s) -- Table A-3 (p. 48): <7, 7-8, 8-9, 9-10, 10-11, total
# - depth classes (m) -- Table B-1 (p. 49): <30, 30-60, 60-700, 700-1000, >1000, total
# - distance to shore (nm) -- Table B-2 (p. 50): <3, 3-12, 12-50, 50-200, total
# - by states
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
  # wind_0 = wind
  
  # wind: categorize and aggregate by energy classes ----
  wind@data = wind@data %>%
    mutate(
      energy_cat = cut(Speed_90, c(0,7:12)),
      energy_lbl = factor(energy_cat, levels(energy_cat), labels=c('<=7', sprintf('%d-%d', 7:11, 8:12))),
      energy_num = factor(energy_cat, levels(energy_cat), labels=7:12) %>% as.character() %>% as.integer())
  
  # dissolve on region & energy
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
