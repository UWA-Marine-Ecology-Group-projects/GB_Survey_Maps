##      ##      ##        ##      ##      ##

## Bathymetry plots ----

### Load libraries ----

library(ggplot2)
library(ggthemes)
library(cowplot)
library(sp)
library(spDta)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(mapview)
library(tmap)
library(mapdata)
library(leaflet)
library(caTools)
library(reshape2)
library(tidyr)
library(car)
library(lattice)
library(latticeExtra)
library(dplyr)
library(raster)
library(rasterVis)
library(zoo)
library(sf)
library(fields)
library(geoR)
library(gstat)
library(ggsn)
library(ggspatial)
library(ggrepel)
library(patchwork)
#library(elsa)
#install.packages("corrplot")
#library(corrplot)
library(broman)
library(viridis)


# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')
p.dir <- paste(w.dir, "plots", sep='/')




# http://oswaldosantos.github.io/ggsn/

# Read gb cmr poly ----
gb <- readOGR(paste(s.dir, "GeoBay.shp",sep='/'))
plot(gb)
crs1 <- proj4string(gb)
levels(gb$ZoneName)
# get poly for each zone --
NPZ <- gb[gb$ZoneName=="National Park Zone",]
HPZ <- gb[gb$ZoneName=="Habitat Protection Zone",]
MUZ <- gb[gb$ZoneName=="Multiple Use Zone",]
SPZ <- gb[gb$ZoneName=="Special Purpose Zone (Mining Exclusion)",]

# read state zones ----
# G:\.shortcut-targets-by-id\1AEL4IR5ZcsB8ti7pHV51StFNn8G_492P\meg-gis\Matt's National No-take Marine Reserves (DRAFT)
#state <- readOGR("G:/.shortcut-targets-by-id/1AEL4IR5ZcsB8ti7pHV51StFNn8G_492P/meg-gis/Matt's National No-take Marine Reserves (DRAFT)/All_StateReserves.shp")

state <- readOGR(paste(s.dir, "state_parks.shp",sep='/'))
plot(state)


# read ngari capes ----

nc <- readOGR(paste(s.dir, "ngari-capes-GBsection.shp",sep='/'))
plot(nc)



# read coastline ----
# G:\.shortcut-targets-by-id\1AEL4IR5ZcsB8ti7pHV51StFNn8G_492P\meg-gis\Matt's National No-take Marine Reserves (DRAFT)
#state <- readOGR("G:/.shortcut-targets-by-id/1AEL4IR5ZcsB8ti7pHV51StFNn8G_492P/meg-gis/Matt's National No-take Marine Reserves (DRAFT)/All_StateReserves.shp")

coast <- readOGR(paste(s.dir, "GB_coastline.shp",sep='/'))
plot(coast)

plot(gb, add=T)
plot(state, add=T)

# Colors 
canary <- brocolors("crayons")["Canary"] # "#ffff99"
mc <- brocolors("crayons")["Melon"] # "#ffbd88"
rs <- brocolors("crayons")["Desert Sand"] # "#ffbd88"


## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html

map <- tm_shape(coast)  + tm_borders(col ='black', lwd = 2) + tm_fill(col = rs) +
  tm_compass(type = "arrow", position = c(0.8, 0.2), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
map

map1 <- map + tm_shape(MUZ) + tm_borders(col ='black', lwd = 2) + tm_fill(col = "#80daeb")

map1

map2 <- map1 + tm_shape(SPZ) + tm_borders(col ='black', lwd = 2) + tm_fill(col = "#dbd7d2")

map2

map3 <- map2 + tm_shape(HPZ) + tm_borders(col ='black', lwd = 2) + tm_fill(col = "#eceabe")

map3

map4 <- map3 + tm_shape(NPZ) + tm_borders(col ='black', lwd = 2) + tm_fill(col = "#93dfb8")

map4

map5 <- map4 + tm_shape(nc) + tm_borders(col ='black', lwd = 2) + tm_fill(col = "#fdbcb4")

map5

map6 <- map5 + tm_shape(state) + tm_borders(col ='black', lwd = 2) + tm_fill(col = "#ffff99")

map6

tmap_save(map6, paste(p.dir, "GB-CMR and state-zones-map.tiff", sep='/'))


## save map ----

tmap_save(map1, paste(p.dir, "GB-MBES.tiff", sep='/'))




### LIDAR data ----

# Read data ----
b <- raster(paste(r.dir, "lidarGB_all_UTM.tif", sep='/'))
plot(b)
proj4string(b)

b2 <- projectRaster(b, crs = crs1)
plot(b2)
plot(gb, add = T)
e <- drawExtent()
b3 <- crop(b2, e)
plot(b3)
plot(gb, add=T)

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html

map <- tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.9, 0.45), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
map

map1 <- tm_shape(b3) + tm_raster(palette=viridis(40, direction =-1), style = 'cont', legend.reverse = TRUE) +
  tm_layout(legend.text.size = 1.7,
            legend.position = c(0.82, 0.1),
            legend.title.color = 'white',
            legend.title.size = 0.11) + map
map1


## save map ----

tmap_save(map1, paste(p.dir, "GB-LIDAR.tiff", sep='/'))



### MBES and LIDAR data ----

# Read data ----
b <- raster(paste(r.dir, "GBmultib_lidar_CMR.tif", sep='/'))
plot(b)
proj4string(b)

plot(gb, add = T)
e <- drawExtent()
b3 <- crop(b, e)
plot(b3)
plot(gb, add=T)

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html

map <- tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.9, 0.59), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
map

map1 <- tm_shape(b3) + tm_raster(palette=viridis(40, direction =-1), style = 'cont', legend.reverse = TRUE) +
  tm_layout(legend.text.size = 1.7,
            legend.position = c(0.82, 0.15),
            legend.title.color = 'white') + map
map1


## save map ----

tmap_save(map1, paste(p.dir, "GB-Fine-bathy.tiff", sep='/'))



### COARSE data ----

# Read data ----
b <- raster(paste(r.dir, "Geog_250mBathy.tif", sep='/'))
plot(b)
proj4string(b)

b2 <-  raster(paste(r.dir, "GB-SW_250mBathy.tif", sep='/'))
plot(b2)
plot(gb, add = T)
e <- drawExtent()
b3 <- crop(b2, e)
plot(b3)
plot(gb, add=T)

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html

map <- tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.9, 0.45), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
map

map1 <- tm_shape(b) + tm_raster(palette=viridis(40, direction =-1), style = 'cont', legend.reverse = TRUE) +
  tm_layout(legend.text.size = 1.7,
            legend.position = c(0.78, 0.15),
            legend.title.color = 'white') + map
map1


## save map ----

tmap_save(map1, paste(p.dir, "GB-coarse-bathy_CMR.tiff", sep='/'))

###

### Plot of GB Zones ----

levels(gb$ZoneName)

# rename zones ---
levels(gb$ZoneName)[levels(gb$ZoneName)=="National Park Zone"] <- "NPZ"
levels(gb$ZoneName)[levels(gb$ZoneName)=="Habitat Protection Zone"] <- "HPZ"
levels(gb$ZoneName)[levels(gb$ZoneName)=="Multiple Use Zone"] <- "MUZ"
levels(gb$ZoneName)[levels(gb$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "SPZ"

levels(gb$ZoneName)


map <- tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_polygons("ZoneName", palette = c("#eceabe", "#80daeb", "#93dfb8" , "#dbd7d2")) +
  tm_compass(type = "arrow", position = c(0.88, 0.54), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1, position = c(0.7, 0.01)) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) +
  tm_layout(legend.text.size = 1,
          legend.position = c(0.82, 0.15),
          legend.title.color = 'white')
map

## save map ----

tmap_save(map, paste(p.dir, "GB-Zones_CMR.tiff", sep='/'))
tmap_save(map, paste(p.dir, "GB-Zones_CMR.png", sep='/'))
