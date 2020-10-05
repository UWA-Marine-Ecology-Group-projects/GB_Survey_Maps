#### Maps of all surveys in GB: BRUVS, AUV, FTV amd DTV --

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
library(tidyverse)
library(dplyr)

## Clear workspace ----
rm(list = ls())

# Set working directory ####
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "plots", sep='/')


## Read GB shapefile ----
gb <- readOGR(paste(s.dir, "GeoBay.shp", sep='/'))
proj4string(gb)

## read WA shapefile ----
coast <- readOGR(paste(s.dir, "GB_coastline.shp", sep='/'))
proj4string(coast)

## read BRUV sp ----
#bruv <- read.csv(paste(d.dir, "BRUVs_2014_CMR.csv", sep='/'))
#str(bruv)
# make as BRUV sp points --
#coordinates(bruv) <- ~coords.x1+coords.x2
#proj4string(bruv) <- proj4string(coast)
# save bruv shapefile --
#writeOGR(bruv, s.dir, "Bruv_CMR", driver = "ESRI Shapefile")
bruv <- readOGR(paste(s.dir, "Bruv_CMR.shp", sep='/'))
plot(gb)
plot(bruv, add=T)


## read FTV sp ----
#ftv <- read.csv(paste(d.dir, "TV_zoning.csv", sep='/'))
#str(ftv)
# make ftv sp points --
#coordinates(ftv) <- ~coords.x1+coords.x2
#proj4string(ftv) <- proj4string(gb)
#plot(gb)
#points(ftv)
#ftv2 <- crop(ftv, gb)
#plot(gb)
#points(ftv2)
# save ftv CMR as shapefile --
#writeOGR(ftv2, s.dir, "Ftv_CMR", driver = "ESRI Shapefile")
ftv <- readOGR(paste(s.dir, "Ftv_CMR.shp", sep='/'))
plot(gb)
plot(ftv, add=T)

## read AUV sp ----
#auv <- read.csv(paste(d.dir, "Auv_zoning.csv", sep='/'))
#str(auv)
# make auv sp points --
#coordinates(auv) <- ~coords.x1+coords.x2
#proj4string(auv) <- proj4string(gb)
#plot(gb)
#points(auv)
# save AUV as shapefile --
#writeOGR(auv, s.dir, "Auv_CMR", drive = "ESRI Shapefile")
auv <- readOGR(paste(s.dir, "Auv_CMR.shp", sep='/'))
plot(gb)
plot(auv, add=T)


## read DTV sp ----
#dtv <- read.csv(paste(d.dir, "DTV_detailed_habitat_dominant.csv", sep='/'))
#str(dtv)
# make dtv sp points --
#coordinates(dtv) <- ~Longitude+Latitude
#proj4string(dtv) <- proj4string(gb)
#plot(gb)
#points(dtv)
#dtv <- crop(dtv, gb)
# save dtv as shapefile --
#writeOGR(dtv, s.dir, "Dtv_CMR", drive = "ESRI Shapefile")
dtv <- readOGR(paste(s.dir, "Dtv_CMR.shp", sep='/'))
plot(gb)
plot(dtv, add=T)
