

library(knitr)


library(dplyr)
covid <- readr::read_csv("data/covid.csv")
muni.coords <- readr::read_csv("data/coords_towns.csv")
covid <- left_join(covid, muni.coords, by = "Municipio")
covid

library(sf)
covid.sf <- st_as_sf(covid)

covid.sf <- st_as_sf(covid, coords = c("x", "y"))
covid.sf

covid.sf

covid.sf <- st_set_crs(covid.sf, value = 25830)
covid.sf

covid.geo <- st_transform(covid.sf, crs = 4326)
covid.geo

st_coordinates(covid.geo)

covid.sf

plot(covid.sf)

plot(covid.sf[0])

plot(covid.sf["Casos"])

plot(covid.sf["Casos"],
     main = "Casos por municipio",
     pch = 16)

library(mapview)
mapview(covid.sf)

mapview(covid.sf, zcol = "Casos")

mapview(covid.sf, zcol = "Fallecidos")

mapview(covid.sf, zcol = "Fallecidos", cex = "Fallecidos")

library(ggplot2)

ggplot(covid.sf) +
  geom_sf() #<<

ggplot(covid.sf) +
  geom_sf() +
  theme_minimal()

ggplot(covid.sf) +
  geom_sf(colour = "blue") + #<<
  theme_minimal()

ggplot(covid.sf) +
  geom_sf(aes(colour = Casos)) +  #<<
  theme_minimal()

ggplot(covid.sf) +
  geom_sf(aes(size = Casos), colour = "orange") +  #<<
  theme_minimal()

ggplot(covid.sf) +
  geom_sf(aes(size = Casos), colour = "orange") +  #<<
  theme_minimal()

ggplot(covid.sf) +
  geom_sf(aes(size = Casos), colour = "orange") +
  cowplot::theme_map() #<<

ggplot(covid.sf) +
  geom_sf(aes(size = Casos), colour = "orange") +
  ggthemes::theme_map() #<<

ggplot(covid.sf) +
  geom_sf(aes(size = Casos), colour = "orange") +
  ggthemes::theme_map() +
  labs(title = "Number of COVID cases per town") #<<

ggplot(covid.sf) +
  geom_sf(aes(size = Casos), colour = "orange") +
  ggthemes::theme_map() +
  labs(title = "Number of COVID cases per town") +
  theme(plot.title = element_text(size = 15, hjust = 0.2), #<<
        legend.title = element_blank()) #<<

ggplot(covid.sf) +
  geom_sf(aes(size = Casos), colour = "orange") +
  ggthemes::theme_map() +
  labs(title = "Number of COVID cases per town") +
  theme(plot.title = element_text(size = 15, hjust = 0.2),
        legend.title = element_blank())  #<<

ggplot(covid.sf) +
  geom_sf(aes(size = Casos), colour = "orange") +
  ggthemes::theme_map() +
  labs(title = "Number of COVID cases per town") +
  theme(plot.title = element_text(size = 15, hjust = 0.2),
        legend.title = element_blank()) +
  scale_size_continuous(breaks = c(0, 1000, 10000, 60000)) #<<

## mymap <- ggplot(covid.sf) +
##   geom_sf(aes(size = Casos), colour = "orange") +
##   ggthemes::theme_map() +
##   labs(title = "Number of COVID cases per town") +
##   theme(plot.title = element_text(size = 15, hjust = 0.2),
##         legend.title = element_blank())
##
## ggsave(filename = "mymap.pdf", plot = mymap, #<<
##        width = 10, height = 7, units = "cm")  #<<

## library(ggiraph) #<<
##
## ggobj <- ggplot(covid.sf) +
##   geom_sf_interactive(aes(size = Casos,  #<<
##                           tooltip = Municipio),
##                       alpha = 0.5) +
##   ggthemes::theme_map(base_size = 8) +
##   theme(legend.position = "bottom")
##
## girafe(ggobj = ggobj, width_svg = 4, height_svg = 3)



library(tmap)

tm_shape(covid.sf) +   # specify spatial object
  tm_symbols()         # map points

tm_shape(covid.sf) +
  tm_symbols(size = "Casos")  #<<

tm_shape(covid.sf) +
  tm_symbols(size = "Casos",
             col = "orange", border.col = "orange") #<<

tm_shape(covid.sf) +
  tm_symbols(size = "Casos",
             col = "orange", border.col = "orange",
             title.size = "COVID cases per town") +
  tm_layout(frame = FALSE, legend.title.size = 0.9) +
  tm_compass(type = "8star", position = c(0.04, 0.2), size = 2) +
  tm_scale_bar(width = 0.15, position = c(0, 0.03))

library(maptiles)
library(terra)

bmap <- get_tiles(covid.sf, zoom = 7, crop = TRUE)
plot(bmap)

bmap <- get_tiles(covid.sf, provider = "CartoDB.Positron", zoom = 7, crop = TRUE)
plot(bmap)

bmap <- get_tiles(covid.sf, provider = "Esri.WorldShadedRelief", zoom = 7, crop = TRUE)
plot(bmap)

bmap <- get_tiles(covid.sf, provider = "Esri.WorldImagery", zoom = 7, crop = TRUE)
plot(bmap)

bmap <- get_tiles(covid.sf, provider = "Stamen.Watercolor", zoom = 7, crop = TRUE)
plot(bmap)

credits <- get_credit("Stamen.Watercolor")
credits

tm_shape(bmap) +
  tm_rgb(alpha = 0.3) +
  tm_shape(covid.sf) +
  tm_symbols(size = "Casos", scale = 1.5, title.size = "Casos por municipio") +
  tm_layout(legend.position = c("right", "bottom"),
            inner.margins = c(0.03, 0.01, 0.02, 0.01)) +
  tm_credits(get_credit("Stamen.Watercolor"), size = 0.4, position = c(0.02, 0))

tm_shape(bmap, bbox = "provincia de Cádiz") + #<<
  tm_rgb(alpha = 0.3) +
  tm_shape(covid.sf) +
  tm_symbols(size = "Casos", scale = 2,
             title.size = "Casos por municipio",
             sizes.legend = c(1000, 10000, 20000)) +
  tm_text(text = "Municipio", size = "Casos",
          legend.size.show = FALSE,
          auto.placement = FALSE, remove.overlap = TRUE, size.lowerbound = 0.5,
          xmod = 1, ymod = 0.7) +
  tm_layout(legend.position = c("left", "bottom"))



map.cases <- tm_shape(covid.sf) +
  tm_symbols(size = "Casos", col = "orange", title.size = "") +
  tm_layout(title = "Cases", title.size = 0.9)


map.deaths <- tm_shape(covid.sf) +
  tm_symbols(size = "Fallecidos", title.size = "") +
  tm_layout(title = "Deaths", title.size = 0.9)

tmap_arrange(map.cases, map.deaths)



tmap_arrange(map.cases, map.deaths, map.deaths, map.cases,
             nrow = 2, ncol = 2)

tmap_mode("view")

tm_shape(covid.sf) +
  tm_symbols(size = "Casos", col = "Casos", id = "Municipio",
             popup.vars = c("Municipio", "Casos"))

## tmap_save(mymap, "mymap.png")  # raster image
##
## tmap_save(mymap, "mymap.svg")  # vector
##
## tmap_save(mymap, "mymap.html")  # html (interactive, animated...)

library(sf)
sort(st_drivers()$name)

munis <- st_read("data/municipios.gpkg")

tmap_mode("plot")

tm_shape(munis) +
  tm_polygons()

object.size(munis)

muni.lite <- tmaptools::simplify_shape(munis, keep.units = TRUE)
object.size(muni.lite)

orig <- tm_shape(munis) +
  tm_polygons()

simple <- tm_shape(muni.lite) +
  tm_polygons()

tmap_arrange(orig, simple)



## sf::st_write(muni.lite, "data/muni.lite.gpkg")

## sf::st_write(muni.lite, "data/muni.lite.shp")

covid.plain <- covid.sf %>%
  st_drop_geometry() %>%
  rename(MUNICIPIO = Municipio)

left_join(muni.lite, covid.plain)

head(muni.lite)

head(covid.sf)

tmap_mode("plot")

tm_shape(muni.lite) +
  tm_polygons(col = "white") +
  tm_shape(covid.sf) +
  tm_dots()

covid.town <- st_join(muni.lite, covid.sf)
covid.town

tm_shape(covid.town) +
  tm_polygons(col = "Casos", border.alpha = 0.05, legend.is.portrait = FALSE)

tm_shape(covid.town) +
  tm_polygons(col = "Fallecidos", border.alpha = 0.05, legend.is.portrait = FALSE)

cases.province <- covid.town %>%
  group_by(PROVINCIA) %>%
  summarise(total.cases = sum(Casos, na.rm = TRUE))
cases.province

tm_shape(cases.province) +
  tm_fill(col = "total.cases")

andal <- st_union(muni.lite)
andal

tm_shape(andal) +
  tm_borders()

andal <- muni.lite %>%
  summarise()
andal

tm_shape(andal) +
  tm_borders() +
  tm_compass()

st_area(andal)

grazalema <- muni.lite %>%
  filter(MUNICIPIO == "Grazalema")
grazalema

sevilla <- cases.province %>%
  filter(PROVINCIA == "Sevilla")
sevilla

st_within(grazalema, sevilla, sparse = FALSE)

st_touches(grazalema, sevilla, sparse = FALSE)

st_is_within_distance(grazalema, sevilla, dist = 10000, sparse = FALSE)

st_is_within_distance(grazalema, sevilla, dist = 50000, sparse = FALSE)

st_distance(grazalema, sevilla)

sevilla.town <- covid.sf %>%
  filter(Municipio == "Sevilla (capital)")
sevilla.town

st_distance(grazalema, sevilla.town)

sev.muni <- muni.lite %>%
  filter(MUNICIPIO == "Sevilla (capital)")

sev.metro <- st_buffer(sev.muni, dist = 20000)
sev.metro

## tmap_mode("view")
##
## tm_shape(sev.metro) +
##   tm_fill() +
##   tm_shape(sev.muni) +
##   tm_fill(col = "black")
##
## tmap_mode("plot")



sev.munis <- muni.lite %>%
  filter(PROVINCIA == "Sevilla")

tm_shape(sev.munis) +
  tm_polygons()

intersect.metro <- st_join(sev.munis, sev.metro, left = FALSE)
intersect.metro

tm_shape(sev.munis) +
  tm_polygons(col = "white") +
  tm_shape(intersect.metro) +
  tm_polygons(col = "yellow") +
  tm_shape(sev.metro) +
  tm_fill(alpha = 0.7)



within.metro <- st_join(sev.munis, sev.metro,
                           join = st_within, left = FALSE)
within.metro

tm_shape(sev.munis) +
  tm_polygons(col = "white") +
  tm_shape(within.metro) +
  tm_polygons(col = "yellow") +
  tm_shape(sev.metro) +
  tm_fill(alpha = 0.7)
