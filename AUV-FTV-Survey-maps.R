


##### MAPS of AUV and FTV Surveys used for Power Analysis ####

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
#library(mapmisc)
#install.packages("ggsn")
devtools::install_github("3wen/legendMap")
library(ggsn)
library(grid)
library(maps)
library(broom)
library(tidyverse)
library(dplyr)
library(ggspatial)


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
