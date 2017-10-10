# load packages, installing if needed ----
packages = c(
  # general data science
  'tidyverse','stringr','units',
  # dynamic document creation
  'knitr','rmarkdown','markdown','bookdown','DT','htmltools','formattable',
  # plotting & mapping
  'RColorBrewer','viridis','scales','tidyverse/ggplot2','ropensci/plotly','maps','mapproj','wch/webshot','geojsonio','rmapshaper',#'albersusa', 'ggsn', ,'plotKML' # 
  # [mapedit](ttp://r-spatial.org/r/2017/01/30/mapedit_intro.html)
  'bhaskarvk/leaflet', 'bhaskarvk/leaflet.extras', 'r-spatial/mapview@develop', 'r-spatial/mapedit','cleangeo','rasterVis',
  # spatial analytical
  'sp','rgdal','raster','ncdf4','rgeos','geosphere','bbest/sf','eblondel/cleangeo','geojsonio','maptools','hrbrmstr/albersusa') 
for (pkg in packages){ # pkg= packages[1] # pkg = 'r-spatial/mapview@develop' # pkg='ropensci/plotly'
  github_pkg = grepl('/', pkg)
  p = ifelse(github_pkg, sub('([-0-9A-Za-z]*)/([-0-9A-Za-z]*)@?([-0-9A-Za-z]*)', '\\2', pkg), pkg)
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
select  = dplyr::select
percent = formattable::percent

# variables & paths ----
d_incr = 100 # depth increment
redo = F
crs_gcs    = leaflet:::epsg4326
crs_gcs_w = '+proj=longlat +datum=WGS84 +lon_wrap=180'


# create_cable-buffer.R paths ----
gdb           = '../data/SubmarineCables/NOAAChartedSubmarineCables.gdb'
depth_nc      = '~/github/obis-lat-time-fig/data/GEBCO_2014_2D.nc'        # 1.87 GB -- too big for Github
depth_grd     = '/Volumes/Best HD/nrel_data_big/data/depth.grd' # wrapped and cropped
depth_m_grd   = '/Volumes/Best HD/nrel_data_big/data/depth_m.grd'
depth_m_cbl3_grd = '/Volumes/Best HD/nrel_data_big/data/depth_m_cbl3.grd'
lns_geo       = '../data/lns.geojson'
lns_usa_geo   = '../data/lns_usa.geojson'
eez_shp       = '~/mbon_data_big/technical/boundaries/eez/eez.shp'
usa_geo       = '../data/eez_usa.geojson'
usa_rgn_geo   = '../data/usa_rgn.geojson'
usa_rgn_s_geo = '../data/usa_rgn_simplify05.geojson'
#usa_rgn_kml   = '../data/usa_rgn.kmz'
lns_d1x_geo   = '../data/lns_d1x.geojson'
lns_d1x_rgn_geo = '../data/lns_d1x_rgn.geojson'
lns_rgn_geo   = '../data/lns_rgn.geojson'
usa_dx_csv    = '../data/usa_lnsdx_ter.csv'
dx2_geo       = sprintf('../data/buf_2xdepth_incr%sm.geojson', d_incr)
dx3_geo       = sprintf('../data/buf_3xdepth_incr%sm.geojson', d_incr)
dx2_kml       = '../data/buf_2xdepth_incr100m.kml'
dx3_kml       = '../data/buf_3xdepth_incr100m.kml'
lns_d1x_kml   = '../data/lns_d1x.kml'
land_usaeez_geo = '../data/land_wrld2_usaeez.geojson'
dx2_depth_geo = sprintf('../data/buf_2xdepth_incr%sm_depth-binned.geojson', d_incr)
dx3_depth_geo = sprintf('../data/buf_3xdepth_incr%sm_depth-binned.geojson', d_incr)

# depth classes
tbl_depth_reclass = tribble(
  ~depth_from, ~depth_to, ~depth_mid,
       -10000,         0,     -5000,
            0,       100,        50,
          100,       200,       150,
          200,      1000,       600,
         1000,     10000,      5000)

# factor labels
depth_levels = c(-5000,      50,       150,         600,     5000)
depth_labels = c( '<0', '0-100', '100-200', '200-1,000', '>1,000')
tide_breaks = c(0,500,1000,1500,10753)
tide_labels = c('0-500','500-1,000','1,000-1,500','>1,500')
wind_labels = c('<=7', '7-8', '8-9', '9-10', '10-11', '11-12')
wave_labels = c('0-10','10-20','20-30','>30')
cable_ord   = c('min_2d'='Facilities (2z)', 'rec_3d'='Cables (3z)', 'rem'='Free')
ter_atl_islands = c('Puerto Rico','US Virgin Islands')
ter_pac_islands = c('Guam','Johnston Atoll','N Mariana Islands','Palmyra Atoll','Wake Island')

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
wind_cbl2_depth_geo = '../data/wind_cable2_depth.geojson'
wind_cbl3_depth_geo = '../data/wind_cable3_depth.geojson'
wind_depth_csv      = '../data/wind_depth.csv'
wind_depth_cbls_csv = '../data/wind_depth_cables.csv'
# wave
wave_shp      = '/Volumes/Best HD/nrel_data_big/nrel.gov/wave/mhk-atlas_wave_wef_ann/wave_wef_ann.shp'
wave_geo      = '../data/wave.geojson'
wave_cbl2_geo = '../data/wave_cable2.geojson'
wave_cbl3_geo = '../data/wave_cable3.geojson'
wave_cbls_csv = '../data/wave_cables.csv'
wave_cbl2_depth_geo = '../data/wave_cable2_depth.geojson'
wave_cbl3_depth_geo = '../data/wave_cable3_depth.geojson'
wave_depth_csv      = '../data/wave_depth.csv'
wave_depth_cbls_csv = '../data/wave_depth_cables.csv'
# tide
tide_shps    = list(
  West = '/Volumes/Best HD/nrel_data_big/nrel.gov/tide/tide_data_west/tide_data_west.shp',
  East = '/Volumes/Best HD/nrel_data_big/nrel.gov/tide/tide_data_east/tide_data_east.shp')
tide_res_dd   = 0.005 # tide raster resolution in decimal degrees
tide_csv      = '../data/tide.csv'
#tide_cbls_csv = '../data/tide_cables.csv'
tide_depth_cbls_csv = '../data/tide_depth_cables.csv'
tbl_form_cbls_csv   = '../data/energy-depth-cables_all.csv'

plot_energy_params = list(
  tide = list(
    energy_labels = tide_labels,
    depth_ranges  = c('0-100'),
    xlab          = "paste('Tidal power (', W/m^2,')')"),
  wave = list(
    energy_labels = wave_labels,
    depth_ranges  = c('0-100','100-200'),
    xlab          = "paste('Wave energy (', kW/m,')')"),
  wind = list(
    energy_labels = wind_labels,
    depth_ranges  = c('0-100','100-200','200-1,000'),
    xlab          = "paste('Wind speed (', m/s,')')"))

