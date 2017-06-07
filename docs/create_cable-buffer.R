library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
#library(geosphere)
library(sf)
select = dplyr::select
library(cleangeo) # devtools::install_github("eblondel/cleangeo")
library(geojsonio)

# working directory
if (basename(getwd()) == 'nrel-cables') setwd('docs')

# variables
d_incr = 100 # depth increment
redo = F # TODO DEBUG

# paths
gdb           = '../data/SubmarineCables/NOAAChartedSubmarineCables.gdb'
#depth_nc    = '../data/big/GEBCO_2014_2D.nc'                            # 1.87 GB -- too big for Github
depth_nc      = '~/github/obis-lat-time-fig/data/GEBCO_2014_2D.nc'        # 1.87 GB -- too big for Github
dx2_rds       = sprintf('../data/buf_2xdepth-incr%sm.rds', d_incr)
dx3_rds       = sprintf('../data/buf_3xdepth-incr%sm.rds', d_incr)
dx2_geo       = sprintf('../data/buf_2xdepth-incr%sm.geojson', d_incr)
dx3_geo       = sprintf('../data/buf_3xdepth-incr%sm.geojson', d_incr)
depth_rc_grd  = '../data/depth_rc.grd'
depth_rcm_grd = '../data/depth_rcm.grd'
lns_rds       = '../data/lns.rds'

if (!file.exists(lns_rds)){
  # cables
  fc = ogrListLayers(gdb)[[1]]
  lns = readOGR(dsn=gdb, layer=fc, verbose=F)
  
  # TODO: move these danglers to Pacific left of existing
  idx = gCentroid(lns, byid=T) %>% coordinates() %>% .[,1] < 100
  lns = subset(lns, idx)
  saveRDS(lns, lns_rds)
} else {
  lns = readRDS(lns_rds)  
}


if (!file.exists(depth_rcm_grd)){
  # load depth
  depth_r = raster(depth_nc, layer = 'elevation')
  
  # convert lines to same projection as depth raster
  lns = spTransform(lns, crs(depth_r))
  
  # buffer out 1 pixel in decimal degrees
  lns_buf1 = rgeos::gBuffer(lns, width=0.008333333)
  
  # extract to extent of submarine cable lines
  depth_rc = crop(depth_r, extent(lns_buf1))
  writeRaster(depth_rc, depth_rc_grd)
  
  # rasterize lines to mask depth
  lns_r = rasterize(lns_buf1, depth_rc, 1)
  
  # mask out cells besides on/near cable lines
  depth_rcm = mask(depth_rc, lns_r)
  
  writeRaster(depth_rcm, depth_rcm_grd)
} else {
  depth_rcm = raster(depth_rcm_grd)
}

if (exists('ply_dx2')) rm(ply_dx2)
# file.remove(dx2_rds); file.remove(dx3_rds)

# generate if needed
if(any(!file.exists(dx2_rds), !file.exists(dx3_rds))){
  
  # setup tiles for iteration
  bb = extent(lns)
  rx = c(floor(bb@xmin), ceiling(bb@xmax))
  ry = c(floor(bb@ymin), ceiling(bb@ymax))
  dx = diff(rx)
  dy = diff(ry)
  nx = 8
  ny = 4
  tx = dx/nx
  ty = dy/ny
  xseq = seq(rx[1], by=tx, length.out=nx)
  yseq = seq(ry[1], by=ty, length.out=ny)
  xyseq = data_frame(xmin=numeric(0), xmax=numeric(0), ymin=numeric(0), ymax=numeric(0))
  for (x in xseq){
    for (y in yseq){
      xyseq = bind_rows(
        xyseq,
        data_frame(
          xmin=x, xmax=x+tx, ymin=y, ymax=y+ty)
      )
    }
  }
  
  # iterate over tiles
  for (i in 1:nrow(xyseq)){ # i = 1
    #for (i in 25:nrow(xyseq)){ # i = 25  # TODO DEBUG
    
    ply_dx2_i_rds = sprintf('../tmp/ply_dx2_%02d.rds',i)
    ply_dx3_i_rds = sprintf('../tmp/ply_dx3_%02d.rds',i)
    
    if (any(!file.exists(ply_dx2_i_rds), !file.exists(ply_dx3_i_rds), redo)){
      
      # setup extent
      e_i = with(xyseq[i,], extent(xmin, xmax, ymin, ymax))
      
      # crop lines
      lns_i = crop(lns, e_i)
      
      # skip tile if not lines
      if (length(lns_i)==0){
        cat(sprintf('%02d: %03.1f,%03.1f -- SKIP: no cables in tile\n', i, e_i@xmin, e_i@ymin))
        next
      }
      cat(sprintf('%02d: %03.1f,%03.1f -- %s\n', i, e_i@xmin, e_i@ymin, Sys.time()))
      
      # clip depth to tile
      depth_i = crop(depth_rcm, e_i)
      depth_i[depth_i > 0] = NA
      depth_i = depth_i * -1
      
      if (all(is.na(getValues(depth_i)))){
        cat('  SKIP: all depth_i is NA\n')
        next
      }
      
      # save for redistribution
      #saveRDS(depth_i, sprintf('../tmp_dist/depth_%02d.rds', i))
      
      # create depth reclass table
      d_max = cellStats(depth_i, 'max')
      if (d_max <= 250){
        mid_depths = 250  
      } else {
        mid_depths = seq(250 + d_incr/2, d_max, by=d_incr)
      }
      tbl_reclass = data_frame(
        depth_from=0, depth_to=250,	depth_1x=250) %>%
        bind_rows(
          data_frame(
            depth_1x = mid_depths) %>%
            mutate(
              depth_from = depth_1x - d_incr/2,
              depth_to   = depth_1x + d_incr/2)) %>%
        mutate(
          depth_2x   = depth_1x*2,
          depth_3x   = depth_1x*3)
      #tbl_reclass %>% DT::datatable() # n=51
      
      # reclassify raster
      cat(sprintf('  reclassify raster -- %s\n', Sys.time()))
      r_d1x = reclassify(depth_i, tbl_reclass %>% select(depth_from, depth_to, depth_1x)) # plot(r_d1x)
      
      # convert raster to vector
      cat(sprintf('  convert to vector -- %s\n', Sys.time()))
      system.time({
        p_d1x = rasterToPolygons(r_d1x, dissolve=T) # tiles:nx = 8;ny = 4 -> 79.2 min [ * 32 rows / 60 min per hr = 42.2 hrs ]
      }) 
      names(p_d1x@data) = 'buf1x'
      
      # project & intersect lns to depth vector
      cat(sprintf('  project & intersect lns to depth vector -- %s\n', Sys.time()))
      lns_t = spTransform(lns, crs(p_d1x))
      system.time({
        lns_i = raster::intersect(lns_t, p_d1x) # tiles:nx = 8;ny = 4 -> 1.7 min [ * 32 rows / 60 min per hr = 53.3 min ]
      })
      
      if (is.null(lns_i)){
        cat(sprintf('  SKIP: is.null(lns_i) -- %s\n', Sys.time()))
        next
      }
      
      # project to Albers Equal Area (with one-sixth rule) for units of meters
      cat(sprintf('  project to Albers -- %s\n', Sys.time()))
      bb = bbox(lns_i)
      lat_range = extendrange(bb['y',],f=-1/6)
      crs_aea = CRS(
        sprintf(
          "+proj=aea +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs",
          lat_range[1], lat_range[2], mean(lat_range), mean(bb['x',])))
      lns_a = spTransform(lns_i, crs_aea) # plot(lns_a)
      
      # iterate over buffer depths
      bufs = lns_i@data %>%
        group_by(buf1x) %>%
        summarize(n = n())
      for (j in 1:nrow(bufs)){ # j = 1
        b = bufs$buf1x[j]
        cat(sprintf('  %03d of %d bufs: %d m -- %s\n', j, nrow(bufs), b, Sys.time()))
        
        lns_b = subset(lns_a, buf1x==b)
        buf_dx2 = gBuffer(lns_b, width=b*2)  
        buf_dx3 = gBuffer(lns_b, width=b*3)  
        
        if (j == 1){
          ply_dx2_i = buf_dx2
          ply_dx3_i = buf_dx3
        } else {
          ply_dx2_i = raster::union(ply_dx2_i, buf_dx2)
          ply_dx3_i = raster::union(ply_dx3_i, buf_dx3)
        }
      } # 3 min
      
      ply_dx2_i = gUnaryUnion(ply_dx2_i)
      ply_dx3_i = gUnaryUnion(ply_dx3_i)
      
      saveRDS(ply_dx2_i, ply_dx2_i_rds)
      saveRDS(ply_dx3_i, ply_dx3_i_rds)
      
    } else {
      ply_dx2_i = readRDS(ply_dx2_i_rds)
      ply_dx3_i = readRDS(ply_dx3_i_rds)
    }
    
    cat(sprintf('%02d -- %s\n', i, Sys.time()))
    ply_dx2_i_gcs_rds = sprintf('../tmp/ply_dx2_%02d_gcs.rds',i)
    ply_dx3_i_gcs_rds = sprintf('../tmp/ply_dx3_%02d_gcs.rds',i)
    
    if (any(!file.exists(ply_dx2_i_gcs_rds), !file.exists(ply_dx3_i_gcs_rds))){
    
      cat(sprintf('  projecting to GCS geometry & projecting -- %s\n', Sys.time()))
      ply_dx2_i_gcs = spTransform(ply_dx2_i, crs(depth_rcm))
      ply_dx3_i_gcs = spTransform(ply_dx3_i, crs(depth_rcm))
      
      # check geometry validity & issues for a sp spatial object
      issues <- clgeo_CollectionReport(ply_dx2_i_gcs) %>% .[.$valid==F,]
      if (nrow(issues)>0){
        cat(sprintf('  cleaning up ply_dx2_i_gcs geometries -- %s\n', Sys.time()))
        print(issues)
        write_csv(issues, sprintf('../tmp/ply_dx2_%02d_gcs_issues.csv',i))
        ply_dx2_i_gcs = clgeo_Clean(ply_dx2_i_gcs, strategy = 'BUFFER')
        issues <- clgeo_CollectionReport(ply_dx2_i_gcs) %>% .[.$valid==F,]
        if (nrow(issues)>0){
          cat(sprintf('  ply_dx2_i_gcs geometries NOT FIXED -- %s\n', Sys.time())) 
        }
      }
      issues <- clgeo_CollectionReport(ply_dx3_i_gcs) %>% .[.$valid==F,]
      if (nrow(issues)>0) {
        cat(sprintf('  cleaning up ply_dx3_i_gcs geometries -- %s\n', Sys.time()))
        print(issues)
        write_csv(issues, sprintf('../tmp/ply_dx3_%02d_gcs_issues.csv',i))
        ply_dx3_i_gcs = clgeo_Clean(ply_dx3_i_gcs, strategy = 'BUFFER')
        issues <- clgeo_CollectionReport(ply_dx3_i_gcs) %>% .[.$valid==F,]
        if (nrow(issues)>0){
          cat(sprintf('  ply_dx3_i_gcs geometries NOT FIXED -- %s\n', Sys.time())) 
        }
      }
      
      cat(sprintf('  writing GCS -- %s\n', Sys.time()))
      saveRDS(ply_dx2_i_gcs, ply_dx2_i_gcs_rds)
      saveRDS(ply_dx3_i_gcs, ply_dx3_i_gcs_rds)
      
    } else {
      cat(sprintf('  reading GCS -- %s\n', Sys.time()))
      ply_dx2_i_gcs = readRDS(ply_dx2_i_gcs_rds)
      ply_dx3_i_gcs = readRDS(ply_dx3_i_gcs_rds)
    }
      
    # merge lines
    cat(sprintf('  union -- %s\n', Sys.time()))
    if (!exists('ply_dx2')){
      ply_dx2 = ply_dx2_i_gcs
      ply_dx3 = ply_dx3_i_gcs
    } else {
      ply_dx2 = raster::union(ply_dx2, ply_dx2_i_gcs)
      ply_dx3 = raster::union(ply_dx3, ply_dx3_i_gcs)
    }
    
  } # finish iterating over tiles
  
  issues <- clgeo_CollectionReport(ply_dx2) %>% .[.$valid==F,]
  if (nrow(issues)>0){
    cat(sprintf('cleaning up ply_dx2 geometries -- %s\n', Sys.time()))
    print(issues)
    write_csv(issues, sprintf('../tmp/ply_dx2_%02d_gcs_issues.csv',i))
    ply_dx2 = clgeo_Clean(ply_dx2, strategy = 'BUFFER')
    issues <- clgeo_CollectionReport(ply_dx2) %>% .[.$valid==F,]
    if (nrow(issues)>0){
      cat(sprintf('  ply_dx2 geometries NOT FIXED -- %s\n', Sys.time())) 
    }
  }
  issues <- clgeo_CollectionReport(ply_dx3) %>% .[.$valid==F,]
  if (nrow(issues)>0) {
    cat(sprintf('cleaning up ply_dx3 geometries -- %s\n', Sys.time()))
    print(issues)
    write_csv(issues, sprintf('../tmp/ply_dx3_%02d_gcs_issues.csv',i))
    ply_dx3 = clgeo_Clean(ply_dx3, strategy = 'BUFFER')
    issues <- clgeo_CollectionReport(ply_dx3) %>% .[.$valid==F,]
    if (nrow(issues)>0){
      cat(sprintf('  ply_dx3 geometries NOT FIXED -- %s\n', Sys.time())) 
    }
  }
  
  # save to filesystem
  saveRDS(ply_dx2, dx2_rds)
  saveRDS(ply_dx3, dx3_rds)
  
  # write to geojson
  geojson_write(ply_dx2, file=dx2_geo)
  geojson_write(ply_dx3, file=dx3_geo)
}

# 09: -151.0,4.0 -- SKIP: no cables in tile
# 13: -136.5,4.0 -- SKIP: no cables in tile
# 17: -122.0,4.0 -- SKIP: no cables in tile
# 20: -122.0,47.5 -- SKIP: no cables in tile
# 21: -107.5,4.0 -- SKIP: no cables in tile
# 23: -107.5,33.0 -- SKIP: no cables in tile
# 24: -107.5,47.5 -- 2017-06-07 09:30:53
# SKIP: all depth_i is NA
# 28: -93.0,47.5 -- 2017-06-07 09:32:47
# SKIP: all depth_i is NA
# 32: -78.5,47.5 -- SKIP: no cables in tile
# There were 44 warnings (use warnings() to see them)
# > warnings()
# Warning messages:
#   1: In raster::union(ply_dx2, ply_dx2_i) : non identical CRS