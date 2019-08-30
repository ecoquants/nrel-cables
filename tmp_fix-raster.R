library(leaflet)
library(raster)
r <- raster(xmn = 165, xmx = 185, ymn = -50, ymx = -31, crs = "+init=epsg:4326", nrows = 50, ncols = 50)
r[] <- rnorm(ncell(r))
r

leaflet() %>% addTiles() %>% addRasterImage(r)

leaflet() %>% addTiles() %>% addRasterImage(rotate(r))
rotate(r)

r_w = shift(rotate(shift(r, 180)), 180) # wrap rotate from [0,360] to [-180,180]

x = shift(r, -360)
leaflet() %>% addTiles() %>% addRasterImage(x)


rotate(r)
