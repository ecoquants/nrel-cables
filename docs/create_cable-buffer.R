# load packages, installing if needed
packages = c('tidyverse','rgdal','raster','ncdf4',
             'rgeos','geosphere','edzer/sfr','eblondel/cleangeo','geojsonio','maptools',
             'leaflet','knitr','rmarkdown','DT','RColorBrewer')
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
#st_drivers()

# working directory
if (basename(getwd()) == 'nrel-cables') setwd('docs')

# variables
d_incr = 100 # depth increment
redo = F # TODO DEBUG

# paths
gdb           = '../data/SubmarineCables/NOAAChartedSubmarineCables.gdb'
#depth_nc    = '../data/big/GEBCO_2014_2D.nc'                            # 1.87 GB -- too big for Github
depth_nc      = '~/github/obis-lat-time-fig/data/GEBCO_2014_2D.nc'        # 1.87 GB -- too big for Github
dx2_geo       = sprintf('../data/buf_2xdepth-incr%sm.geojson', d_incr)
dx3_geo       = sprintf('../data/buf_3xdepth-incr%sm.geojson', d_incr)
depth_m_grd   = '../data/depth_m.grd'
lns_geo       = '../data/lns.geojson'
lns_usa_geo   = '../data/lns_usa.geojson'
eez_shp       = '~/mbon_data_big/technical/boundaries/eez/eez.shp'
usa_geo       = '../data/eez_usa.geojson'
lns_d1x_geo   = '../data/lns_d1x.geojson'

# cables
if (!file.exists(lns_geo)){
  fc = ogrListLayers(gdb)[[1]]
  lns = readOGR(dsn=gdb, layer=fc, verbose=F) %>% 
    st_as_sf()
  write_sf(lns, lns_geo)
} else {
  lns = read_sf(lns_geo)  
}

# USA EEZ
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

if (!file.exists(lns_usa_geo)){
  system.time({
    lns_usa = st_intersection(lns, usa) # 10.1 min
  })
  #file.remove('../data/lns_usa.geojson')
  write_sf(lns_usa, lns_usa_geo)
}
lns_usa = read_sf(lns_usa_geo)

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

#clean_geo = function(geo, sfx='clean-buffer'){
clean_geo = function(geo, sfx='dirty', mv_dirty=T){
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

if (exists('ply_dx2')) rm(ply_dx2)
# file.remove(dx2_rds); file.remove(dx3_rds)

# generate if needed
if(any(!file.exists(dx2_geo), !file.exists(dx3_geo))){
  
  lns_d1x = read_sf('../data/lns_d1x.geojson')
  
  # iterate over usa parts
  parts = unique(lns_d1x$part)
  for (i in 1:length(parts)){ # i = 1
    #for (i in 25:nrow(tiles_sf)){ # i = 25  # TODO DEBUG
    p = parts[i]
    p_name = lns_d1x %>% filter(part == p) %>% head(1) %>% .$GeoName
    
    ply_dx2_i_geo = sprintf('../tmp/ply_dx2_i%02d.geojson',i)
    ply_dx3_i_geo = sprintf('../tmp/ply_dx3_i%02d.geojson',i)
    
    if (any(!file.exists(ply_dx2_i_geo), !file.exists(ply_dx3_i_geo), redo)){
      
      cat(sprintf('%02d: part #%02d %s -- %s\n', i, p, p_name, Sys.time()))
      
      # setup extent
      lns_i = lns_d1x %>% 
        filter(part == p)
      
      cat(sprintf('  project to Albers -- %s\n', Sys.time()))
      bb = st_bbox(lns_i)
      lat_range = extendrange(bb[c('ymin','ymax')], f=-1/6) # 5.796732 8.983678
      crs_aea = sprintf(
        "+proj=aea +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs",
        lat_range[1], lat_range[2], mean(lat_range), mean(bb[c('xmin','xmax')]))
      lns_a = st_transform(lns_i, crs_aea)
      
      # iterate over buffer depths
      bufs = lns_a %>%
        as_data_frame() %>%
        group_by(buf1x) %>%
        summarize(n = n())
      for (j in 1:nrow(bufs)){ # j=1 # j=2
        b = bufs$buf1x[j]
        cat(sprintf('  %03d of %d bufs: %d m -- %s\n', j, nrow(bufs), b, Sys.time()))
        
        lns_b = lns_a %>% filter(buf1x==b)
        
        # buf_dx2 = st_buffer(lns_b, dist=b*2)  
        # buf_dx3 = st_buffer(lns_b, dist=b*3)  
        buf_dx2 = gBuffer(lns_b %>% as('Spatial'), width=b*2)  
        buf_dx3 = gBuffer(lns_b %>% as('Spatial'), width=b*3)  
        
        if (j == 1){
          ply_dx2_i = buf_dx2
          ply_dx3_i = buf_dx3
        } else {
          # suppressWarnings({
          #   ply_dx2_i = st_union(ply_dx2_i, buf_dx2)
          #   ply_dx3_i = st_union(ply_dx3_i, buf_dx3)
          # })
          ply_dx2_i = raster::union(ply_dx2_i, buf_dx2)
          ply_dx3_i = raster::union(ply_dx3_i, buf_dx3)
        }
      }
      
      #ply_dx2_i = st_union(ply_dx2_i)
      #ply_dx3_i = st_union(ply_dx3_i)
      ply_dx2_i = gUnaryUnion(ply_dx2_i)
      ply_dx3_i = gUnaryUnion(ply_dx3_i)
      
      # st_write(ply_dx2_i, ply_dx2_i_geo)
      # st_write(ply_dx3_i, ply_dx3_i_geo)
      st_write(ply_dx2_i %>% st_as_sf(), ply_dx2_i_geo, quiet=T)
      st_write(ply_dx3_i %>% st_as_sf(), ply_dx3_i_geo, quiet=T)
    } else {
      #ply_dx2_i = st_read(ply_dx2_i_geo)
      #ply_dx3_i = st_read(ply_dx3_i_geo)
      ply_dx2_i = st_read(ply_dx2_i_geo, quiet=T) %>% as('Spatial')
      ply_dx3_i = st_read(ply_dx3_i_geo, quiet=T) %>% as('Spatial')
    }
    
    ply_dx2_i_gcs_geo = sprintf('../tmp/ply_dx2_%02d_gcs.geojson',i)
    ply_dx3_i_gcs_geo = sprintf('../tmp/ply_dx3_%02d_gcs.geojson',i)
     
    if (any(!file.exists(ply_dx2_i_gcs_geo), !file.exists(ply_dx3_i_gcs_geo))){
    
      cat(sprintf('  projecting to GCS geometry & projecting -- %s\n', Sys.time()))
      # ply_dx2_i_gcs = st_transform(ply_dx2_i, leaflet:::epsg4326)
      # ply_dx3_i_gcs = st_transform(ply_dx3_i, leaflet:::epsg4326)
      ply_dx2_i_gcs = st_transform(ply_dx2_i %>% st_as_sf(), leaflet:::epsg4326) %>% mutate(buffer='2*depth')
      ply_dx3_i_gcs = st_transform(ply_dx3_i %>% st_as_sf(), leaflet:::epsg4326) %>% mutate(buffer='3*depth')
      
      st_write(ply_dx2_i_gcs, ply_dx2_i_gcs_geo, quiet=T)
      st_write(ply_dx3_i_gcs, ply_dx3_i_gcs_geo, quiet=T)
      
      ply_dx2_i_gcs = ply_dx2_i_gcs %>% as('Spatial')
      ply_dx3_i_gcs = ply_dx3_i_gcs %>% as('Spatial')

    }
    ply_dx2_i_gcs = st_read(ply_dx2_i_gcs_geo, quiet=T) %>% as('Spatial') # %>% mutate(id=1)
    ply_dx3_i_gcs = st_read(ply_dx3_i_gcs_geo, quiet=T) %>% as('Spatial')
    
    #cat(sprintf('%02d of %d parts -- %s\n', i, length(parts), Sys.time()))
    # clean_geo(ply_dx2_i_gcs_geo)
    # clean_geo(ply_dx3_i_gcs_geo)
    #if (length(list.files('../tmp', '.*after.*', full.names=T)) > 0) stop('found "after" issues cleaning in ../tmp')
    
    
    # merge
    cat(sprintf('  union part %02d -- %s\n', i, Sys.time()))
    if (!exists('ply_dx2')){
      ply_dx2 = ply_dx2_i_gcs
      ply_dx3 = ply_dx3_i_gcs
    } else {
      ply_dx2 = gUnion(ply_dx2, ply_dx2_i_gcs)
      ply_dx3 = gUnion(ply_dx3, ply_dx3_i_gcs)
    }

  } # finish iterating over parts

  cat(sprintf('union all -- %s\n', Sys.time()))
  ply_dx2 = gUnaryUnion(ply_dx2)
  ply_dx3 = gUnaryUnion(ply_dx3)

  cat(sprintf('intersect all with eez -- %s\n', Sys.time()))
  # add data frame
  ply_dx2 = SpatialPolygonsDataFrame(ply_dx2, data_frame(buffer='depth*2'))
  ply_dx3 = SpatialPolygonsDataFrame(ply_dx3, data_frame(buffer='depth*3'))
  
  # intersect with USA EEZ
  usa_sp = usa %>% select(eez=GeoName, eez_part=part) %>% as('Spatial')
  system.time({
    ply_dx2 = raster::intersect(ply_dx2, usa_sp)
    ply_dx3 = raster::intersect(ply_dx3, usa_sp)
  }) # 26 min
  
  cat(sprintf('write all -- %s\n', Sys.time()))
  ply_dx2 %>% st_as_sf() %>% write_sf(dx2_geo, quiet=T)
  ply_dx3 %>% st_as_sf() %>% write_sf(dx3_geo, quiet=T)
  
  cat(sprintf('clean all -- %s\n', Sys.time()))
  clean_geo(dx2_geo)
  clean_geo(dx3_geo)
  
  # time estimate for script: 73 min 
  # TODO: simplify as native geojson
}
cat(sprintf('FINISHED -- %s\n', Sys.time()))

# Google Earth sanity check with measurement tool
read_sf(dx2_geo) %>% write_sf('../data/buf_2xdepth-incr100m.kml')
read_sf(dx3_geo) %>% write_sf('../data/buf_3xdepth-incr100m.kml')
read_sf(lns_usa_geo) %>% write_sf('../data/lns_usa.kml')
#file.remove('../data/lns_d1x.kml')
read_sf(lns_d1x_geo) %>% select(buf1x, part, GeoName) %>% write_sf('../data/lns_d1x.kml')
