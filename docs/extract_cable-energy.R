# load packages, installing if needed
packages = c('tidyverse','rgdal','raster','geosphere',
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

# variables
d_incr = 100 # depth increment

# paths
dx2_geo       = sprintf('../data/buf_2xdepth-incr%sm.geojson', d_incr)
dx3_geo       = sprintf('../data/buf_3xdepth-incr%sm.geojson', d_incr)
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

# TODO: stacked histogram by cbl2/(cbl3-cbl2)/other; hist(wind$Speed_90)
#
# Musial et al (2016): technical potential 7 - 11.5 m/s
# - wind speed (m/s) -- Table A-3 (p. 48): <7, 7-8, 8-9, 9-10, 10-11, total
# - depth classes (m) -- Table B-1 (p. 49): <30, 30-60, 60-700, 700-1000, >1000, total
# - distance to shore (nm) -- Table B-2 (p. 50): <3, 3-12, 12-50, 50-200, total
# - by states

# read cables ----
cbl2 = read_sf(dx2_geo) %>%
  select(-buffer) %>% 
  as('Spatial')
cbl3 = read_sf(dx3_geo) %>% 
  select(-buffer) %>% 
  as('Spatial')

# read wind ----
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
  
  # categorize and aggregate wind speeds ----
  wind@data = wind@data %>%
    mutate(
      speed_cat = cut(Speed_90, c(0,7:12)),
      speed_lbl = factor(speed_cat, levels(speed_cat), labels=c('<=7', sprintf('%d-%d', 7:11, 8:12))),
      speed_num = factor(speed_cat, levels(speed_cat), labels=7:12) %>% as.character() %>% as.integer())
  
  # dissolve on region & speed
  wind = raster::aggregate(wind, by=c('region','speed_lbl','speed_num'))
  
  # calculate area
  wind$total_km2 = areaPolygon(wind) / (1000*1000)
  # wind@data %>% %>% summary # check for NAs
  
  # write to geojson
  wind %>% st_as_sf() %>% write_sf(wind_geo)
}
wind = read_sf(wind_geo) %>% as('Spatial')

# intersect wind with cables & dissolve on region & speed -----
if (any(!file.exists(wind_cbl2_geo), !file.exists(wind_cbl3_geo))){
  wind_cbl2 = raster::intersect(wind, cbl2) %>%
    raster::aggregate(by=c('region','speed_lbl','speed_num'))
  wind_cbl3 = raster::intersect(wind, cbl3) %>%
    raster::aggregate(by=c('region','speed_lbl','speed_num'))
  
  # calculate area
  wind_cbl2$cable2_km2 = areaPolygon(wind_cbl2) / (1000*1000)
  wind_cbl3$cable3_km2 = areaPolygon(wind_cbl3) / (1000*1000)
  
  # write to geojson
  wind_cbl2 %>% st_as_sf() %>% write_sf(wind_cbl2_geo)
  wind_cbl3 %>% st_as_sf() %>% write_sf(wind_cbl3_geo)
}
wind_cbl2 = read_sf(wind_cbl2_geo) %>% as('Spatial')
wind_cbl3 = read_sf(wind_cbl3_geo) %>% as('Spatial')

# summarize wind with cables into table -----
if (!file.exists(wind_cbls_csv)){
  
  wind_cbls = wind@data %>%
    left_join(
      wind_cbl3@data,
      by=c('region','speed_lbl','speed_num')) %>%
    left_join(
      wind_cbl2@data, 
      by=c('region','speed_lbl','speed_num')) %>%
    replace_na(list(
      cable2_km2 = 0, 
      cable3_km2 = 0)) %>%
    rename(
      area_km2 = total_km2) %>%
    mutate(
      cable2_pct = cable2_km2 / area_km2 * 100,
      cable3_pct = cable3_km2 / area_km2 * 100) %>%
    group_by(region) %>%
    mutate(
      pct_region        = area_km2 / sum(area_km2) * 100,
      cable2_pct_region = cable2_km2 / sum(area_km2) * 100,
      cable3_pct_region = cable3_km2 / sum(area_km2) * 100) %>%
    ungroup() %>%
    select(region, speed_num, speed_lbl, area_km2, pct_region, cable2_km2, cable2_pct, cable2_pct_region, cable3_km2, cable3_pct, cable3_pct_region)
    arrange(region, speed_num)

  wind_cbls %>% write_csv(wind_cbls_csv)
}

