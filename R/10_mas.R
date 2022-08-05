##Aula 01 (26/07/22) Andrea Sanchez-Tapias
#library(sf)
#library(tmap)
#library(dplyr)

data(World)
# package tmap has a syntax similar to ggplot. The functions start all with tm_
tm_shape(World) +
  tm_borders()

#head(World)
names(World)
class(World)
dplyr::glimpse(World)


plot(World[2])
plot(World[,2])


plot(World[23,])
plot(World[23,"well_being"])

head(sf::st_coordinates(World))
no_geom <- sf::st_drop_geometry(World)
class(no_geom)
st_bbox(World)

names(World)
unique(World$continent)

World %>%
  filter(continent == "South America") %>%
  tm_shape() +
  tm_borders()

World %>%
  mutate(our_countries = if_else(iso_a3 %in% "BRA", "green") %>%
  tm_shape() +
  tm_borders()

World %>%
  filter(continent == "South America") %>%
  mutate(our_countries = if_else(iso_a3 %in% "BRA", "green") %>%
  tm_shape() +
  tm_borders()

World %>%
  mutate(our_countries = if_else(iso_a3 %in% c("COL","BRA", "MEX"), "red", "grey")) %>%
  tm_shape() +
  tm_borders() +
  tm_fill(col = "our_countries") +
  tm_add_legend("fill",
                "Countries",
                col = "red")

#Loading, plotind and saving a shape from the disk
#library(rnaturalearth)
#library(rnaturalearthhires)
bra <- ne_states(country = "brazil", returnclass = "sf")
plot(bra)

dir.create("data/shapefiles", recursive = TRUE)
st_write(obj = bra, dsn = "data/shapefiles/bra.shp", delete_layer = TRUE)


bra2 <- read_sf("data/shapefiles/bra.shp")
class(bra)
class(bra2)

plot(bra)
plot(bra2)

#Loading, ploting, and saving a raster from the disk
#library(raster)
dir.create(path = "data/raster/", recursive = TRUE)
tmax_data <- getData(name = "worldclim", var = "tmax", res = 10, path = "data/raster/")
plot(tmax_data)

is(tmax_data) #the data are a raster stack, several rasters piled
dim(tmax_data)
extent(tmax_data)
res(tmax_data)


#Check the tutorials in (rspatialdata)
