######## Depth strata polygons #######
library(ggplot2)
library(ggthemes)
library(extrafont)
library(broman) # for colors: https://kbroman.files.wordpress.com/2014/05/crayons.png
library(raster)
library(sp)
library(sf)
library(rgdal)
library(plyr)
library(maptools)
library(broom)


## Clear workspace ----
rm(list = ls())

# Set working directory ####
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
d.dir <- paste(w.dir, "data", sep='/')
# Set graph directory - to save plots
p.dir <- paste(w.dir, "plots", sep='/')


## Read polygons of depth strata used for the balanced design ----
# Use sf library to read shapefiles and use st_union
#d1 <- readOGR(paste(w.dir, "shapefiles", "Depth strata", "CMR_Depth1A.shp", sep='/'))
d1 <- st_read(paste(w.dir, "shapefiles", "Depth strata", "CMR_Depth1A.shp", sep='/'))
plot(d1)
proj4string(d1)
d1
# the GRIDCODE has the depth values DEPTH RANGE: 5-21 m 
max(d1$GRIDCODE)
min(d1$GRIDCODE)
# combine all little polygons into one
d1 <- st_union(d1)
plot(d1)

#d2 <- readOGR(paste(w.dir, "shapefiles", "Depth strata", "CMR_Depth2A.shp", sep='/'))
d2 <- st_read(paste(w.dir, "shapefiles", "Depth strata", "CMR_Depth2A.shp", sep='/'))
plot(d2)
proj4string(d2)
d2
# the GRIDCODE has the depth values DEPTH RANGE: 23-30 m 
max(d2$GRIDCODE)
min(d2$GRIDCODE)
# combine all little polygons into one
d2 <- st_union(d2)
plot(d2)

#d3 <- readOGR(paste(w.dir, "shapefiles", "Depth strata", "CMR_Depth3A.shp", sep='/'))
d3 <- st_read(paste(w.dir, "shapefiles", "Depth strata", "CMR_Depth3A.shp", sep='/'))
#SPZ <- st_read(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_SPZ.shp", sep='/'))
plot(d3)
proj4string(d3)
d3$GRIDCODE
# the GRIDCODE has the depth values DEPTH RANGE: 30-38 m
max(d3$GRIDCODE)
min(d3$GRIDCODE)
# combine all little polygons into one
d3 <- st_union(d3)
plot(d3)

#d4 <- readOGR(paste(w.dir, "shapefiles", "Depth strata", "CMR_Depth4A.shp", sep='/'))
d4 <- st_read(paste(w.dir, "shapefiles", "Depth strata", "CMR_Depth4A.shp", sep='/'))
plot(d4)
proj4string(d4)
d4
# the GRIDCODE has the depth values DEPTH RANGE: 39-47 m
max(d4$GRIDCODE)
min(d4$GRIDCODE)
# combine all little polygons into one
d4 <- st_union(d4)
plot(d4)
d4 # +proj=longlat +datum=WGS84 +no_defs

## Plot them all:
plot(d1, col= "yellow")
plot(d2, add=T, col = "green")
plot(d3, add=T, col = "blue")
plot(d4, add=T, col="dark blue")

## Save the new combined polygons ###
st_write(d1, paste(w.dir, "shapefiles", "Depth strata", "Depth1-5-20m.shp", sep='/'))

st_write(d2, paste(w.dir, "shapefiles", "Depth strata", "Depth2-20-30m.shp", sep='/'))

st_write(d3, paste(w.dir, "shapefiles", "Depth strata", "Depth3-30-40m.shp", sep='/'))

st_write(d4, paste(w.dir, "shapefiles", "Depth strata", "Depth4-40-50m.shp", sep='/'))


### Load zone shapefiles ----
MPw <- readOGR(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_MNPZ_West.shp", sep='/'))
#MPw <- st_read(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_MNPZ_West.shp", sep='/'))
plot(MPw)
str(MPw)
proj4string(MPw) # "+proj=utm +zone=50 +south +ellps=GRS80 +units=m +no_defs"
MPw <- spTransform(MPw, CRS("+proj=longlat +datum=WGS84 +no_defs"))

MPe <- readOGR(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_MNPZ_East.shp", sep='/'))
#MPe <- st_read(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_MNPZ_East.shp", sep='/'))
plot(MPe)
str(MPe)
proj4string(MPe)
MPe <- spTransform(MPe, CRS("+proj=longlat +datum=WGS84 +no_defs"))

SPZ <- readOGR(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_SPZ.shp", sep='/'))
#SPZ <- st_read(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_SPZ.shp", sep='/'))
plot(SPZ)
proj4string(SPZ)
SPZ <- spTransform(SPZ, CRS("+proj=longlat +datum=WGS84 +no_defs"))

MUZ <- readOGR(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_MUZ.shp", sep='/'))
#MUZ <- st_read(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_MUZ.shp", sep='/'))
plot(MUZ )
proj4string(MUZ )
MUZ <- spTransform(MUZ, CRS("+proj=longlat +datum=WGS84 +no_defs"))


# Plot little map of the depth strata ----

## Read shp new GB zoning ----
gb <- readOGR(paste(w.dir, "shapefiles", "GeoBay.shp", sep='/'))
plot(gb)
proj4string(gb) # "+proj=longlat +ellps=GRS80 +no_defs"
proj4string(gb) <- "+proj=longlat +datum=WGS84 +no_defs"

plot(gb)
plot(d1, add=T, col= "yellow")
plot(d2, add=T, col = "green")
plot(d3, add=T, col = "blue")
plot(d4, add=T, col="dark blue")
plot(MUZ, border = "black", lwd=3, add=T)
plot(SPZ, border = "black", lwd=3, add=T)
plot(MPe, border = "black", lwd=3, add=T)
plot(MPw, border = "black", lwd=3, add=T)
