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


# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "raster", sep='/')
p.dir <- paste(w.dir, "plots", sep='/')




# http://oswaldosantos.github.io/ggsn/

# Read gb cmr poly ----
gb <- readOGR(paste(s.dir, "GeoBay.shp",sep='/'))
plot(gb)
levels(gb$ZoneName)
# get poly for each zone --
NPZ <- gb[gb$ZoneName=="National Park Zone",]
HPZ <- gb[gb$ZoneName=="Habitat Protection Zone",]
MUZ <- gb[gb$ZoneName=="Multiple Use Zones",]
SPZ <- gb[gb$ZoneName=="Special Purpose Zone (Mining Exclusion)",]

# Pick colors ----
sg <- brocolors("crayons")["Fern"] # "#78dbe2"
alg <-  brocolors("crayons")["Raw Umber"] # "#1dacd6" 
sand <-  brocolors("crayons")["Unmellow Yellow"] # "#f75394"

pal1 <- c(sand, sg, alg )




#### ####     FINE BATHY PREDICTIONS     #### 

####    BRUVs Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-Bruvs.tif", sep='/'))
plot(pred)

# raster as data frame --
#redf <- as.data.frame(pred, xy=TRUE) %>% na.omit()
#head(predf)

# fix class levels for plotting --
xx <-levels(pred)[[1]]
xx
class(xx)
xx <- xx[-1,]
xx$ID <- xx$ID[xx$ID != "0",]
xx$ID <- c('3','2', '1')
xx$category <- c('Unconsolidated',  'Seagrasses', 'Algae' )
levels(pred) <- xx
pred
#xx <-levels(pred)[[1]]

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html


map1 <- tm_shape(pred) + tm_raster(palette=pal1)

map2 <- map1 + tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.84, 0.25), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.text.size = 1,
            legend.position = c(0.76, 0.1),
            legend.title.color = 'white') +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map2
class(map2)