library(knitr)


library(geodata)
tmin.sp <- worldclim_country(country = "Spain", var = "tmin", path = "data/")

include_graphics("images/rasters.png")

include_graphics("images/cube1.png")

library(terra)
tmin.sp <- rast("data/tminSp.tif")
tmin.sp

plot(tmin.sp)

plot(tmin.sp, y = 1)

plot(tmin.sp, y = 7)

library(RColorBrewer)
plot(tmin.sp, y = 1,
     col = brewer.pal(9, "Blues"))

plot(tmin.sp, y = 1,
     col = RColorBrewer::brewer.pal(9, "Blues"),
     axes = FALSE)

andal.sf <- sf::st_read("data/Andalucia_contorno.gpkg", quiet = TRUE)  #<<
plot(tmin.sp, y = 1, axes = FALSE,
     col = RColorBrewer::brewer.pal(9, "Blues"))
plot(vect(andal.sf), add = TRUE)  #<<

andal <- vect("data/Andalucia_contorno.gpkg")  #<<
plot(tmin.sp, y = 1, axes = FALSE,
     col = RColorBrewer::brewer.pal(9, "Blues"))
plot(andal, add = TRUE)  #<<

## pdf("mymap.pdf")
##
## plot(tmin.sp, y = 1, col = brewer.pal(9, "Blues"))
##
## dev.off()

library(tmap)
tm_shape(tmin.sp) +
  tm_raster()

tm_shape(tmin.sp) +
  tm_raster(palette = "Blues",
            title = "Minimum\nTemperature") +
  tm_layout(legend.position = c(0.05, 0.03),
            panel.label.bg.color = "white",
            panel.labels = month.name)

## tmin.jan <- subset(tmin.sp, 1)
## writeRaster(tmin.jan, "data/tmin.jan.tif")

tmin.jan <- rast("data/tmin.jan.tif")
tm_shape(tmin.jan) +
  tm_raster(palette = "Blues", title = "January\ntemperature")

tm_shape(tmin.jan) +
  tm_raster(palette = "Blues", title = "January\ntemperature") +
  tm_shape(andal.sf) +
  tm_borders()

## map <- tm_shape(tmin.jan) +
##   tm_raster(palette = "Blues", title = "January\ntemperature")
##
## tmap_save(map, "mytmap.png")   #<<

library(rasterVis)
levelplot(tmin.jan)

levelplot(tmin.jan, margin = FALSE, par.settings = BuRdTheme())

library(ggplot2)
gplot(tmin.jan) +   #<<
  geom_tile(aes(fill = value)) +
  cowplot::theme_map()

names(tmin.sp) <- month.name
levelplot(tmin.sp, par.settings = BuRdTheme())

library(geodata)
tmax.sp <- worldclim_country(country = "Spain", var = "tmax", path = "data/")
tmax.sp

tmin.jan <- subset(tmin.sp, 1)

tmax.jan <- subset(tmax.sp, 1)

tmean.jan <- mean(tmin.jan, tmax.jan)

tjan <- c(tmin.jan, tmean.jan, tmax.jan)
names(tjan) <- c("Tmin", "Tmean", "Tmax")
levelplot(tjan, layout = c(3, 1), par.settings = BuRdTheme())

tmean.sp <- mean(tmin.sp, tmax.sp)
tmean.sp

names(tmean.sp) <- month.name
levelplot(tmean.sp, par.settings = BuRdTheme())

## writeRaster(tmean.sp, "data/tmeanSp.tif")

tmin.jan.crop <- crop(tmin.jan, ext(-8, -1, 36, 39))
plot(tmin.jan.crop)

library(sf)
and.lim <- vect(andal.sf)
tmin.jan.crop <- crop(tmin.jan, and.lim)
plot(tmin.jan.crop)

tmin.jan.crop <- mask(tmin.jan.crop, and.lim)
plot(tmin.jan.crop)

tmin.year <- mean(tmin.sp)
plot(tmin.year, col = brewer.pal(n = 9, "Blues"))

tmin.winter <- subset(tmin.sp, c("January", "February", "March"))
tmin.winter.avg <- min(tmin.winter)
plot(tmin.winter.avg)

tmin.andal <- crop(tmin.sp, and.lim)
tmin.andal <- mask(tmin.andal, and.lim)
levelplot(tmin.andal, par.settings = BuRdTheme())


global(tmin.andal, "min", na.rm = TRUE)

global(tmin.andal, "mean", na.rm = TRUE)

library(dplyr)
andal.muni <- st_read("data/municipios_lite.gpkg", quiet = TRUE)
andal.provs <- andal.muni %>%
  group_by(PROVINCIA) %>%
  summarise()
plot(andal.provs)

andal.provs.geo <- st_transform(andal.provs, crs = 4326)

tmin.prov <- extract(tmin.andal, vect(andal.provs.geo))  #<<
head(tmin.prov)

tmin.prov <- extract(tmin.andal, vect(andal.provs.geo),
                     fun = "mean", na.rm = TRUE)  #<<
round(tmin.prov, 1)

aemet <- st_read("data/aemet/Estaciones_Automaticas.shp", quiet = TRUE)
# https://www.miteco.gob.es/es/cartografia-y-sig/ide/descargas/otros/default.aspx
head(aemet)

tm_shape(aemet) +
  tm_dots()

aemet <- st_transform(aemet, crs = 4326)
head(aemet)

aemet.andal <- st_filter(aemet, andal.sf)
tm_shape(aemet.andal) +
  tm_dots()

aemet.tmin <- extract(tmin.jan, vect(aemet.andal))   #<<
aemet.tmin <- bind_cols(aemet.andal, aemet.tmin)
head(aemet.tmin)

## library(geodata)
## elev <- elevation_30s(country = "Spain", path = "data/")

elev <- rast("data/elevSp.tif")

plot(elev)

aemet.elev <- extract(elev, vect(aemet.andal))
aemet.tmin.elev <- bind_cols(aemet.tmin, aemet.elev)
ggplot(aemet.tmin.elev) +
  geom_point(aes(elevSp, January))

library(mgcv)
model <- gam(January ~ s(elevSp), data = aemet.tmin.elev)
model

visreg::visreg(model)

elev.andal <- crop(elev, and.lim)
elev.andal <- mask(elev.andal, and.lim)
levelplot(elev.andal, margin = FALSE)

tmin.pred <- predict(elev.andal, model)
tmin.pred

levelplot(tmin.pred, margin = FALSE, par.settings = BuRdTheme())

grazalema <- andal.muni %>%
  filter(MUNICIPIO == "Grazalema") %>%
  st_transform(crs = 4326)

elev.gra <- elevatr::get_elev_raster(grazalema, z = 13)
elev.gra <- rast(elev.gra)
plot(elev.gra)

plot(elev.gra)
plot(as.contour(elev.gra), add = TRUE)

slopes <- terrain(elev.gra, "slope", unit = "radians")
plot(slopes)

aspect <- terrain(elev.gra, "aspect", unit = "radians")
plot(aspect)

## # Plot hillshading as basemap
## hs <- shade(slopes, aspect)
## plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = FALSE)
##
## # overlay with elevation
## plot(elev.gra, col = terrain.colors(25), alpha = 0.5, legend = FALSE, axes = FALSE, add = TRUE)
##
## # add contour lines
## contour(elev.gra, col = "grey40", add = TRUE)

include_graphics("images/elev_gra.png")

## library(rayshader)
## library(rayvista)
##
## ## Grazalema limits (polygon)
## grazalema <- andal.muni %>%
##   filter(MUNICIPIO == "Grazalema") %>%
##   st_transform(crs = 4326)
##
## graz.3D <- plot_3d_vista(req_area = grazalema)

include_graphics("images/grazalema3D.png")

## library(rayshader)
## library(rayvista)
## lapalmaTF <- plot_3d_vista(
##   lat = 28.719946, long = -17.867091, radius = 30000,
##   overlay_detail = 13, overlay_alpha = 0.6,
##   elevation_detail = 11, show_vista = FALSE)
##
## lapalmaTF$dem_matrix %>%
##   height_shade() %>%
##   add_shadow(ray_shade(lapalmaTF$dem_matrix, zscale = 20), 0.2) %>%
##   add_overlay(., lapalmaTF$texture,rescale_original = TRUE) %>%
##   plot_3d(., lapalmaTF$dem_matrix, zscale = 20,
##           windowsize = 1200, zoom = 0.25, phi = 30, theta = 45)
##
## render_snapshot(clear = TRUE)

include_graphics("images/rayvista_lapalma.png")

include_graphics("images/terrainr.jpg")

elev.gra <- crop(elev, grazalema)
elev.gra
elev.pts <- as.points(elev.gra)
elev.pts

elev.pts.utm <- project(elev.pts, "epsg:25830")

## elev.pts.utm.sf <- elev.pts %>%
##   st_as_sf() %>%
##   st_transform(crs = 25830) %>%
##   vect()

ras <- rast(elev.pts.utm, resolution = 1000,
            vals = elev.pts.utm$elevSp)
ras

plot(ras)

landsat <- system.file("raster/landsat.tif", package = "spDataLarge")
landsat <- rast(landsat)
landsat

plot(landsat)

plotRGB(landsat, r = 3, g = 2, b = 1)

ndvi <- (landsat[[4]] - landsat[[3]]) / (landsat[[4]] + landsat[[3]])
ndvi

## plotRGB(landsat, r = 3, g = 2, b = 1)

include_graphics("images/rgb.png")

tm_shape(ndvi) + tm_raster()
