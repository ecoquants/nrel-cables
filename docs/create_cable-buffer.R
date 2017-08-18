# TODO: next steps

# working directory
if (basename(getwd()) == 'nrel-cables') setwd('docs')

# load packages and variables ----
source('./packages_vars.R')
redo = T

clean_geo = function(geo, sfx='dirty', mv_dirty=T){
  # clean up geometry
  # inputs:
  # - geo: geometry file like geojson, readable by read_sf()
  # output:
  # - geo_clean.geojson
  # - geo_clean_issues-before.csv
  # - geo_clean_issues-after.csv
  # geo = '../tmp/ply_dx2_13_gcs.geojson'; sfx='dirty'; mv_dirty=T
  v = read_sf(geo) %>% as('Spatial')
  issues <- clgeo_CollectionReport(v) %>% .[.$valid==F,]
  if (nrow(issues)>0){
    cat(sprintf('  %d issue(s) before %s...\n', nrow(issues), sfx))
    print(issues)
    write_csv(issues, sprintf('%s_%s-issues-before.csv', tools::file_path_sans_ext(geo), sfx))
    
    if (mv_dirty){
      geo_dirty = sprintf('%s_%s.geojson', tools::file_path_sans_ext(geo), sfx)
      file.rename(geo, geo_dirty)
      geo_clean = geo
    } else {
      geo_clean = sprintf('%s_%s.geojson', tools::file_path_sans_ext(geo), sfx)
    }
    
    v2 = clgeo_Clean(v, strategy = 'BUFFER')
    v2 %>% st_as_sf() %>% write_sf(geo_clean)
    
    issues <- clgeo_CollectionReport(v2) %>% .[.$valid==F,]
    if (nrow(issues)>0){
      cat(sprintf('  %d issue(s) after %s...\n', nrow(issues), sfx))
      print(issues)
      write_csv(issues, sprintf('%s_%s-issues-after.csv', tools::file_path_sans_ext(geo), sfx))
    } else {
      cat('  issues FIXED\n')
    }
  }
}

# dissolve
dissolve1 = function(x, crs_str){
  x %>% st_as_sf() %>% mutate(one=1) %>% as('Spatial') %>%
    gUnaryUnion() %>% 
    SpatialPolygonsDataFrame(data_frame(one=1)) %>% 
    st_as_sf() %>% st_set_crs(crs_str)
}

# merge
merge2 = function(x, y, drop_cols_except_one=T){
  raster::union(
    x %>% st_as_sf() %>% mutate(one=1) %>% as('Spatial'), 
    y %>% st_as_sf() %>% mutate(one=1) %>% as('Spatial')) %>%
    st_as_sf() %>%
    select(geometry) %>%
    mutate(
      one=1)
}

# read cables ----
if (!file.exists(lns_geo)){
  fc = ogrListLayers(gdb)[[1]]
  lns = readOGR(dsn=gdb, layer=fc, verbose=F) %>% 
    st_as_sf()
  write_sf(lns, lns_geo)
} else {
  lns = read_sf(lns_geo)  
}

# read USA EEZ ----
if (!file.exists(usa_geo)){
  #file.remove(usa_geo)
  read_sf(eez_shp) %>% 
    filter(Sovereign1 == 'United States') %>%
    st_cast('POLYGON') %>%
    mutate(
      part = row_number()) %>%
    write_sf(usa_geo)
}
usa = read_sf(usa_geo)

# intersect cables with eez ----
if (!file.exists(lns_usa_geo)){
  system.time({
    lns_usa = st_intersection(lns, usa) # 10.1 min
  })
  #file.remove('../data/lns_usa.geojson')
  write_sf(lns_usa, lns_usa_geo)
}
lns_usa = read_sf(lns_usa_geo)

# depth raster masked to cables buffer ----
if (!file.exists(depth_m_grd)){
  # load depth
  depth = raster(depth_nc, layer = 'elevation')
  
  # buffer out 1 pixel in decimal degrees
  lns_buf1 = st_buffer(lns_usa, dist=0.008333333)
  
  # rasterize lines to mask depth
  system.time({
    lns_r = rasterize(as(lns_buf1, 'Spatial'), depth, 1) # 292.4/60 min
  })
  
  # mask out cells besides on/near cable lines
  system.time({
    depth_m = mask(depth, lns_r) # 123.4/60 min
  })
  
  # convert to bathymetric depth
  system.time({
    depth_m[depth_m > 0] = NA
    depth_m = depth_m * -1
  }) # 465.2/60 min
  
  system.time({
    writeRaster(depth_m, '../data/depth_m.tif', overwrite=T)  # 230.1/60 min
  })
  system.time({
    writeRaster(depth_m, depth_m_grd, overwrite=T)  # 100.5/60 min
  })
} else {
  depth_m = raster(depth_m_grd)
}

# intersect cables with raster ----- 
if (!file.exists(lns_d1x_geo)){
  
  # create depth reclass table
  d_max = cellStats(depth_m, 'max')
  tbl_reclass = data_frame(
    depth_from=0, depth_to=250,	depth_1x=250) %>%
    bind_rows(
      data_frame(
        depth_1x = seq(250 + d_incr/2, d_max, by=d_incr)) %>%
        mutate(
          depth_from = depth_1x - d_incr/2,
          depth_to   = depth_1x + d_incr/2)) %>%
    mutate(
      depth_2x   = depth_1x*2,
      depth_3x   = depth_1x*3)
  
  write_csv(tbl_reclass, '../data/tbl_reclass.csv')
  
  # reclassify raster
  system.time({
    r_d1x = reclassify(depth_m, tbl_reclass %>% select(depth_from, depth_to, depth_1x)) # 104.2/60 min
  })
  system.time({
    writeRaster(r_d1x, '../data/r_d1x.tif', overwrite=T)  # 206.1/60 min
  })
  
  # convert raster to vector
  system.time({
    p_d1x = rasterToPolygons(r_d1x, dissolve=T) # 179.4/60 min
  }) 
  names(p_d1x@data) = 'buf1x'
  system.time({
    p_d1x %>% st_as_sf() %>% write_sf('../data/p_d1x.geojson') # 3.5 sec
  })
  
  # intersect lns to depth vector
  system.time({
    lns_d1x = raster::intersect(lns_usa %>% as('Spatial'), p_d1x) # 274.8/60 min
  })
  #file.remove('../data/lns_d1x.geojson')
  lns_d1x %>% st_as_sf() %>% write_sf(lns_d1x_geo)
} else {
  lns_d1x = read_sf(lns_d1x_geo)
}

# aggregate 5:22pm
usa0_geo = '../tmp/usa0.geojson'
if (!file.exists(usa0_geo)){

  # [Tidy spatial data in R: using dplyr, tidyr, and ggplot2 with sf](http://strimas.com/r/tidy-sf/)

  usa0 = read_sf(eez_shp) %>% 
    filter(
      Sovereign1 == 'United States',
      Territory1 %in% unique(lns_d1x$Territory1))

  usa0_sp = usa0 %>% as('Spatial')
  #usa %>% as.data.frame() %>% select(-geometry) %>% View()
  if (!gIsValid(usa0_sp)) usa0_sp = clgeo_Clean(usa0_sp, strategy='BUFFER')
  # Ring Self-intersection at or near point -72.026275780000006 41.263013059999999
  usa0_sp %>% st_as_sf() %>% write_sf(usa0_geo)
} else {
  usa0 = read_sf(usa0_geo)
}
# TODO: ?editFeatures leaflet w/ albersusa in 


# %>% # usa parts with  
#   mutate(
#     territory = Territory1,
#     territory = ifelse(part==19, 'US West', territory),
#     territory = ifelse(part==18, 'US East', territory)) %>%
#   group_by(territory) %>%
#   summarise(
#     n_parts = n()) %>%
#   as('Spatial') %>%
#   clgeo_Clean(strategy = 'BUFFER') %>%
#   st_as_sf()
# 
# clgeo_CollectionReport(usa_cbl %>% as('Spatial')) %>% .[.$valid==F,]
# usa_cbl = clgeo_Clean(usa_cbl, strategy = 'BUFFER')

# [Marine Regions · Gulf of Mexico (IHO Sea Area)](http://www.marineregions.org/gazetteer.php?p=details&id=4288)
gom_shp_url = 'http://geo.vliz.be/geoserver/wfs?request=getfeature&service=wfs&version=1.0.0&typename=MarineRegions:iho&outputformat=SHAPE-ZIP&filter=%3CPropertyIsEqualTo%3E%3CPropertyName%3Eid%3C%2FPropertyName%3E%3CLiteral%3E26%3C%2FLiteral%3E%3C%2FPropertyIsEqualTo%3E'
tmp_zip = tempfile(fileext='.zip')
download.file(gom_shp_url, tmp_zip)
tmp_dir = tempdir()
unzip(tmp_zip, exdir=tmp_dir) # list.files(tmp_dir)
gom = read_sf(file.path(tmp_dir, 'iho.shp'))
gom_sp = gom %>% as('Spatial')
gIsValid(gom_sp)

# clgeo_CollectionReport(gom %>% as('Spatial')) %>% .[.$valid==F,]
# gom = clgeo_Clean(gom, strategy = 'BUFFER')

east_notgom_geo = '../tmp/east_notgom_sp.geojson'

east = usa %>% filter(part==18)
east_sp = east %>% as('Spatial')
if (!gIsValid(east_sp)) east_sp = clgeo_Clean(east_sp, strategy='BUFFER')

east_notgom_ply = gDifference(east_sp, gom_sp)

east_notgom_sp = SpatialPolygonsDataFrame(
  Sr          = east_notgom_ply,
  data        = data_frame(id=1))


east_notgom_sp %>%
  st_as_sf() %>%
  write_sf(east_notgom_geo)

east_notgom = read_sf(east_notgom_geo)
# [mapedit](http://r-spatial.org/r/2017/01/30/mapedit_intro.html)

east_notgom_parts = east_notgom %>%
  st_cast('POLYGON') %>%
  mutate(
    part    = row_number(),
    area    = st_area(geometry),
    ctr_lon = st_centroid(geometry) %>% st_coordinates() %>% .[,1])

gom_extra = east_notgom_parts %>% filter(ctr_lon < -83)
gom_extra_sp = gom_extra %>% as('Spatial')

gom_plus_geo = '../tmp/gom_plus.geojson'
gom_plus = merge2(gom_sp, gom_extra_sp)
gom_plus_d = dissolve1(gom_plus, crs_gcs)
gIsValid(gom_plus %>% as('Spatial'))
write_sf(gom_plus_d, gom_plus_geo, delete_dsn=T)
gom_plus = read_sf(gom_plus_geo)

east_notgom2_geo = '../tmp/east_notgom2.geojson'
east_notgom2 = east_notgom_parts %>% 
  filter(ctr_lon >= -83) %>%
  dissolve1(crs_gcs)
write_sf(east_notgom2, east_notgom2_geo, delete_dsn=T)
east_notgom2 = read_sf(east_notgom2_geo)

#mapview(east_notgom2)
#mapview(gom_plus)
#mapview(usa %>% filter(part==19))

# # old
# ply %>%
# as('Spatial') %>%
#   SpatialPolygonsDataFrame(
#     Sr          = gUnaryUnion(., id=.$Territory1),
#     data        = data_frame(ID=.$Territory1), match.ID=T)

# usa_rgn_0 = usa_rgn
usa_rgn_sp = usa %>% 
  # Contiguous US West
  filter(part==19) %>%
  mutate(
    territory = 'West') %>%
  select(territory) %>%
  as('Spatial') %>%
  raster::union(
    # Gulf of Mexico
    gom_plus %>%
      mutate(
        territory = 'Gulf of Mexico') %>%
      select(territory) %>%
      as('Spatial')) %>%
  raster::union(
    # Contiguous US East, except Gulf of Mexico
    east_notgom2 %>%
      mutate(
        territory = 'East') %>%
      select(territory) %>%
      as('Spatial')) %>%
  raster::union(
    # remaining US territories overlapping with cables
    usa0 %>%
      select(territory = Territory1) %>%
      filter(territory != 'United States') %>%
      as('Spatial'))
  # SLOW
# Too few points in geometry component at or near point -81.708715389999995 24.57471949

if (!gIsValid(usa_rgn_sp)) usa_rgn_sp = clgeo_Clean(usa_rgn_sp, strategy='BUFFER')

#usa_rgn_sp = usa_rgn
usa_rgn = usa_rgn_sp %>% st_as_sf()
usa_rgn = usa_rgn %>%
  mutate(
    territory = ifelse(
      !is.na(territory), 
      territory,
      ifelse(
        !is.na(territory.1),
        territory.1,
        ifelse(
          !is.na(territory.2),
          territory.2,
          NA))) %>%
      recode(
        'United States Virgin Islands'='US Virgin Islands',
        'Northern Mariana Islands'='N Mariana Islands')) %>%
  select(territory)


# merge Puerto
usa_nopr = usa_rgn %>%
  filter(territory!='Puerto Rico') %>%
  as('Spatial')
usa_pr = usa_rgn %>%
  filter(territory=='Puerto Rico') %>%
  as('Spatial') %>%
  dissolve1(crs_gcs) %>%
  as('Spatial')
usa_rgn2 = raster::union(usa_nopr, usa_pr) %>%
  st_as_sf() %>%
  mutate(
    territory = ifelse(is.na(territory), 'Puerto Rico', territory)) %>%
  select(territory) %>%
  arrange(territory)
gIsValid(usa_rgn2 %>% as('Spatial'))

usa_rgn_geo = '../data/usa_rgn.geojson'
write_sf(usa_rgn2, usa_rgn_geo, delete_dsn=T)
usa_rgn = read_sf(usa_rgn_geo)

usa_rgn2
plot(usa_rgn2['territory'])
#mapview(usa_rgn, zcol='territory')

if (exists('ply_dx2')) rm(ply_dx2)
# file.remove(dx2_rds); file.remove(dx3_rds)

# generate if needed
if(any(!file.exists(dx2_geo), !file.exists(dx3_geo), redo)){
  
  lns_d1x = read_sf('../data/lns_d1x.geojson')
  
  # iterate over usa eez parts ----
  #parts = unique(lns_d1x$part) # DEBUG
  parts = usa %>% 
    filter(
      part %in% unique(lns_d1x$part),
      !Territory1 %in% c('Hawaii','Alaska')) %>% 
    .$part
  # usa %>% filter(part==p) %>% .$Territory1
  for (i in 1:length(parts)){ # p=18; i=which(parts==p)
    #for (i in 25:nrow(tiles_sf)){ # i = 25  # TODO DEBUG
    p = parts[i]
    p_name = lns_d1x %>% filter(part == p) %>% head(1) %>% .$GeoName
    
    ply_dx2_p_geo = sprintf('../tmp/ply_dx2_p%02d.geojson',p)
    ply_dx3_p_geo = sprintf('../tmp/ply_dx3_p%02d.geojson',p)
    
    #ply_dx2_p = read_sf(ply_dx2_p_geo)
    #mapview(ply_dx2_p)
    if (any(!file.exists(ply_dx2_p_geo), !file.exists(ply_dx3_p_geo), redo)){
      
      cat(sprintf('%02d of %d: part #%02d %s -- %s\n', i, length(parts), p, p_name, Sys.time()))
      
      # setup extent & project to Albers for buffering in meters
      lns_i = lns_d1x %>% 
        filter(part == p)
      # lns_i_geo = sprintf('../tmp/lns_p%2d.geojson', p)
      # write_sf(lns_i, lns_i_geo)
      # clean_geo(lns_i_geo)
      
      cat(sprintf('  project to Albers -- %s\n', Sys.time()))
      bb = st_bbox(lns_i)
      lat_range = extendrange(bb[c('ymin','ymax')], f=-1/6) # 5.796732 8.983678
      crs_aea = sprintf(
        "+proj=aea +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs",
        lat_range[1], lat_range[2], mean(lat_range), mean(bb[c('xmin','xmax')]))
      lns_a = st_transform(lns_i, crs_aea)
      
      # dx2 = read_sf(dx2_geo) 
      # dx3 = read_sf(dx3_geo) 
      # badbuf = mapview(list(dx2, lns_d1x)) %>%
      #   editMap()
      # plot(badbuf$finished['X_leaflet_id'])
      # plot(badbuf2$finished['X_leaflet_id'])
      # gomex_badbox = badbuf$finished %>%
      #    slice(1)
      # gomex_badbox_geo = '../tmp/gomex_badbox.geojson'
      # # write_sf(gomex_badbox, gomex_badbox_geo)
      # gomex_badbox = read_sf(gomex_badbox_geo)
      # badboxes = gomex_badbox
      # badboxes_a = st_transform(badboxes, crs_aea)
      # badboxes_a_sp = badboxes_a %>% as('Spatial')
      #
      # #plot(gomex_badbox)
      # badbuf2 = mapview(list(dx2, lns_d1x), maxpoints=2e6) %>%
      #   editMap()
      # mapview(list(dx3, dx2, lns_d1x, gomex_test$finished))
      
      # iterate over buffer depths
      bufs = lns_a %>%
        as_data_frame() %>%
        group_by(buf1x) %>%
        summarize(n = n())
      for (j in 1:nrow(bufs)){ # j=1 # j=2
        b = bufs$buf1x[j]
        cat(sprintf('  %03d of %d bufs: %d m -- %s\n', j, nrow(bufs), b, Sys.time()))
        
        lns_b = lns_a %>% filter(buf1x==b)
        lns_b_sp = lns_b %>% as('Spatial') # plot(lns_b_sf['part'])
        
        # buffer ----
        buf_dx2 = gBuffer(lns_b_sp, width=b*2)  
        buf_dx3 = gBuffer(lns_b_sp, width=b*3)  
        
        # # check if bad buffer before union
        # buf_x_bad = rgeos::gIntersects(buf_dx2, badboxes_a_sp)
        # cat(sprintf('    before union\n', length(buf_x_bad)))
        # if (buf_x_bad) browser()
        
        if (j == 1){
          ply_dx2_p = buf_dx2
          ply_dx3_p = buf_dx3
        } else {
          ply_dx2_p = merge2(ply_dx2_p, buf_dx2)
          ply_dx3_p = merge2(ply_dx3_p, buf_dx3)
        }
        
        # # check if bad buffer after union
        # buf_x_bad = rgeos::gIntersects(ply_dx2_p, badboxes_a_sp)
        # cat(sprintf('    after union\n', length(buf_x_bad)))
        # if (buf_x_bad) browser()
        #ply_dx2_p_0 = ply_dx2_p # ply_dx2_p = ply_dx2_p_0
        #ply_dx3_p_0 = ply_dx3_p # ply_dx3_p = ply_dx3_p_0
        # plot(ply_dx2_p); plot(ply_dx3_p)
        
      }
      
      ply_dx2_p = dissolve1(ply_dx2_p, crs_aea)
      ply_dx3_p = dissolve1(ply_dx3_p, crs_aea)
      # plot(ply_dx2_p); plot(ply_dx3_p)
      
      # write temp geo
      st_write(ply_dx2_p, ply_dx2_p_geo, quiet=T, delete_dsn=T)
      st_write(ply_dx3_p, ply_dx3_p_geo, quiet=T, delete_dsn=T)
    } else {
      ply_dx2_p = st_read(ply_dx2_p_geo, quiet=T, crs=crs_aea)
      ply_dx3_p = st_read(ply_dx3_p_geo, quiet=T, crs=crs_aea)
      # plot(ply_dx2_p); plot(ply_dx3_p)
    }
    
    ply_dx2_p_gcs_geo = sprintf('../tmp/ply_dx2_p%02d_gcs.geojson',p)
    ply_dx3_p_gcs_geo = sprintf('../tmp/ply_dx3_p%02d_gcs.geojson',p)
    
    if (any(!file.exists(ply_dx2_p_gcs_geo), !file.exists(ply_dx3_p_gcs_geo), redo)){
      
      cat(sprintf('  projecting to gcs, ie geographic coordinate system as lon/lat in WGS84 -- %s\n', Sys.time()))
      ply_dx2_p_gcs = st_transform(ply_dx2_p, crs_gcs) %>% mutate(buffer='2*depth')
      ply_dx3_p_gcs = st_transform(ply_dx3_p, crs_gcs) %>% mutate(buffer='3*depth')
      
      st_write(ply_dx2_p_gcs, ply_dx2_p_gcs_geo, quiet=T, delete_dsn=T)
      st_write(ply_dx3_p_gcs, ply_dx3_p_gcs_geo, quiet=T, delete_dsn=T)
      
    }
    ply_dx2_p_gcs = st_read(ply_dx2_p_gcs_geo, quiet=T, crs=crs_gcs) %>% select(-one)
    ply_dx3_p_gcs = st_read(ply_dx3_p_gcs_geo, quiet=T, crs=crs_gcs) %>% select(-one)
    # plot(ply_dx2_p_gcs); plot(ply_dx3_p_gcs)
    
    # merge cable buffers by eez parts ----
    cat(sprintf('  union part %02d -- %s\n', i, Sys.time()))
    if (!exists('ply_dx2')){
      ply_dx2 = ply_dx2_p_gcs
      ply_dx3 = ply_dx3_p_gcs
    } else {
      # i=8; p=15; clgeo_CollectionReport(ply_dx2_p_gcs) %>% .[.$valid==F,]
      # usa %>% filter(part==p) %>% .$Territory1
      #ply_dx2_p_gcs = dissolve1(ply_dx2_p_gcs, crs_gcs)
      #ply_dx2 = dissolve1(ply_dx2, crs_gcs)
      
      ply_dx2 = merge2(ply_dx2, ply_dx2_p_gcs)
      ply_dx3 = merge2(ply_dx3, ply_dx3_p_gcs)
    }
    
  } # finish iterating over parts
  
  # dissolve all
  cat(sprintf('union all -- %s\n', Sys.time()))
  ply_dx2 = dissolve1(ply_dx2, crs_gcs)
  # FIX!
  ply_dx3 = clgeo_Clean(ply_dx3 %>% as('Spatial'), strategy = 'BUFFER')
  ply_dx3 = dissolve1(ply_dx3, crs_gcs)
  
  #
  #Error in gUnaryUnion(.) : 
  #  TopologyException: Input geom 1 is invalid: Self-intersection at or near point -124.97150226260101 38.159075210133764
  
  # # clip and identify eez
  # cat(sprintf('intersect all with eez -- %s\n', Sys.time()))
  # # add data frame
  # ply_dx2 = SpatialPolygonsDataFrame(ply_dx2, data_frame(buffer='depth*2'))
  # ply_dx3 = SpatialPolygonsDataFrame(ply_dx3, data_frame(buffer='depth*3'))
  # 
  # # intersect with USA EEZ ----
  # usa_sp = usa %>% select(eez=GeoName, eez_part=part) %>% as('Spatial')
  # system.time({
  #   ply_dx2 = raster::intersect(ply_dx2, usa_sp)
  #   ply_dx3 = raster::intersect(ply_dx3, usa_sp)
  # }) # 26 min
  
  cat(sprintf('write all -- %s\n', Sys.time()))
  ply_dx2 %>% write_sf(dx2_geo, quiet=T, delete_dsn=T)
  ply_dx3 %>% write_sf(dx3_geo, quiet=T, delete_dsn=T)
  
  cat(sprintf('clean all -- %s\n', Sys.time()))
  clean_geo(dx2_geo)
  clean_geo(dx3_geo)
  
  # add area_km2
  read_sf(dx2_geo) %>%
    mutate(
      area_km2 = st_area(geometry) / (1000 * 1000)) %>%
    write_sf(dx2_geo, delete_dsn=T)
  read_sf(dx3_geo) %>%
    mutate(
      area_km2 = st_area(geometry) / (1000 * 1000)) %>%
    write_sf(dx3_geo, delete_dsn=T)
  
  # time estimate for script: 73 min 
}
cat(sprintf('FINISHED -- %s\n', Sys.time()))

# Google Earth sanity check with measurement tool ----
file.remove(dx2_kml, dx3_kml, lns_d1x_kml)
read_sf(dx2_geo) %>% write_sf(dx2_kml, delete_dsn=T)
read_sf(dx3_geo) %>% write_sf(dx3_kml, delete_dsn=T)
#file.remove('../data/lns_d1x.kml')
read_sf(lns_d1x_geo) %>% select(buf1x, part, GeoName) %>% write_sf(lns_d1x_kml, delete_dsn=T)

# fix GoMex horizontal
# dx2 = read_sf(dx2_geo) 
# gomex_bbox = mapview(dx2) %>%
#   editMap()
# 
# lns_d1x = read_sf('../data/lns_d1x.geojson')
# gomex_lns = mapview(lns_d1x) %>%
#   editMap()
# 
# lns_d1x

# fix Gulf of Mexico (gom / GoMex / gomex) in usa_rgn ----
usa_rgn = read_sf(usa_rgn_geo)
usa_s_rgn = read_sf(usa_rgn_s_geo)

eez = read_sf(eez_shp)  %>%
eez = eez %>%
  mutate(geometry = (geometry + c(360,90)) %% c(360) - c(0,90)) %>% 
  st_set_crs(crs_gcs_w)
  
eez_us = eez %>%
  filter(GeoName == "United States Exclusive Economic Zone")
usa_rgn_gom = usa_rgn %>%
  filter(territory == "Gulf of Mexico")

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data=usa_rgn_gom, fillColor = 'red') %>%
#   addPolygons(data=eez_us, fillColor = 'green')

usa_rgn_gom2 = usa_rgn_gom %>%
  st_intersection(eez_us)

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data=usa_rgn_gom2, fillColor = 'red')

usa_rgn2 = usa_rgn %>%
  filter(territory != "Gulf of Mexico") %>%
  rbind(
    usa_rgn_gom2 %>%
      select(territory, area_km2))

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data=usa_rgn2, fillColor = 'red')

usa_rgn2_s = ms_simplify(usa_rgn2 %>% as('Spatial')) %>% st_as_sf()

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data=usa_rgn2_s, fillColor = 'red')

file.remove(usa_rgn_geo)
file.remove(usa_rgn_s_geo)
write_sf(usa_rgn2, usa_rgn_geo)
write_sf(usa_rgn2_s, usa_rgn_s_geo)
