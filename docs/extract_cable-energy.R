# working directory
if (basename(getwd()) == 'nrel-cables') setwd('docs')

# load packages and variables ----
source('./packages_vars.R')

# cables: read ----
cbl2 = read_sf(dx2_geo) %>%
  select(-buffer) %>%
  as('Spatial')
cbl3 = read_sf(dx3_geo) %>%
  select(-buffer) %>%
  as('Spatial')
cbl_prj = st_crs(cbl2 %>% st_as_sf())$proj4string


# [Marine Regions · Gulf of Mexico (IHO Sea Area)](http://www.marineregions.org/gazetteer.php?p=details&id=4288)
gom_geo_url = 'http://geo.vliz.be/geoserver/wfs?request=getfeature&service=wfs&version=1.1.0&typename=MarineRegions:iho&outputformat=json&filter=%3CPropertyIsEqualTo%3E%3CPropertyName%3Eid%3C%2FPropertyName%3E%3CLiteral%3E26%3C%2FLiteral%3E%3C%2FPropertyIsEqualTo%3E'
gom_kml = 'http://geo.vliz.be/geoserver/wms?request=GetMap&service=wms&version=1.1.1&srs=EPSG:4326&layers=MarineRegions:iho&width=800&height=376&bbox=-98.0539218184371,17.4068080184755,-80.4330407378397,31.464843750339&styles=gazetteer_red&Format=KML&filter=%3CPropertyIsEqualTo%3E%3CPropertyName%3Eid%3C%2FPropertyName%3E%3CLiteral%3E26%3C%2FLiteral%3E%3C%2FPropertyIsEqualTo%3E'

tmp_geo = tempfile(fileext='.geojson')
download.file(gom_geo_url, tmp_geo)
gom_sf = read_sf(tmp_geo)
gom = read_sf(gom_kml) %>% # st_cast('MULTIPOLYGON')
  
gom2 =  st_union(gom)

plot(gom2)  
  aggregate(gom['Name'], list(Name=Name), first)
  #slice(1) %>%
  


gom = 

names(gom)

ggplot(gom2) +
  geom_sf(fill='gray')
# plot(gom['Name']) # SLOW!
# TODO: http://r-spatial.org/r/2017/01/30/mapedit_intro.html


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
