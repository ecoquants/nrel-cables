# load packages, installing if needed ----
packages = c(
  # general data science
  'tidyverse',  
  # dynamic document creation
  'knitr','rmarkdown','bookdown','DT', 
  # plotting & mapping
  'RColorBrewer','leaflet', 
  # spatial analytical
  'rgdal','raster','ncdf4','rgeos','geosphere','edzer/sfr','eblondel/cleangeo','geojsonio','maptools') 
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

# variables & paths ----
d_incr = 100 # depth increment
redo = F

# create_cable-buffer.R paths ----
gdb           = '../data/SubmarineCables/NOAAChartedSubmarineCables.gdb'
depth_nc      = '~/github/obis-lat-time-fig/data/GEBCO_2014_2D.nc'        # 1.87 GB -- too big for Github
depth_m_grd   = '../data/depth_m.grd'
lns_geo       = '../data/lns.geojson'
lns_usa_geo   = '../data/lns_usa.geojson'
eez_shp       = '~/mbon_data_big/technical/boundaries/eez/eez.shp'
usa_geo       = '../data/eez_usa.geojson'
lns_d1x_geo   = '../data/lns_d1x.geojson'
dx2_geo       = sprintf('../data/buf_2xdepth-incr%sm.geojson', d_incr)
dx3_geo       = sprintf('../data/buf_3xdepth-incr%sm.geojson', d_incr)

# extract_cable-energy.R paths ----
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