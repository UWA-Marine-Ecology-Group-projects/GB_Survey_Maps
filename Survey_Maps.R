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

## read GB coast shapefile ----
#w <- readOGR(paste(s.dir, "WA_wgs84.shp", sep ='/'))
#plot(w)
#e <- drawExtent()
#w <- crop(w,e)
#plot(w)
#plot(gb, add=T)
#writeOGR(w, dsn = s.dir, "GB_coastline", driver = "ESRI Shapefile", overwrite_layer = T)
coast <- readOGR(paste(s.dir, "GB_coastline.shp", sep='/'))
proj4string(coast)
plot(coast)

## read BRUV data ----
bruv <- read.csv(paste(d.dir, "BRUVs_2014_CMR.csv", sep='/'))
#str(bruv)
# make as BRUV sp points --
#coordinates(bruv) <- ~coords.x1+coords.x2
#proj4string(bruv) <- proj4string(coast)
# save bruv shapefile --
#writeOGR(bruv, s.dir, "Bruv_CMR", driver = "ESRI Shapefile")
#bruv <- readOGR(paste(s.dir, "Bruv_CMR.shp", sep='/'))
#plot(gb)
#plot(bruv, add=T)


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
# save ftv as csv --
#ftv3 <- as.data.frame(ftv2)
#str(ftv3)
#write.csv(ftv3, paste(d.dir, "Ftv_CMR.csv", sep='/'))
ftv <- read.csv(paste(d.dir, "Ftv_CMR.csv", sep='/'))
str(ftv)
# save ftv CMR as shapefile --
#writeOGR(ftv2, s.dir, "Ftv_CMR", driver = "ESRI Shapefile")
#ftv <- readOGR(paste(s.dir, "Ftv_CMR.shp", sep='/'))
#plot(gb)
#plot(ftv, add=T)

## read AUV sp ----
auv <- read.csv(paste(d.dir, "Auv_zoning.csv", sep='/'))
str(auv)
# make auv sp points --
#coordinates(auv) <- ~coords.x1+coords.x2
#proj4string(auv) <- proj4string(gb)
#plot(gb)
#points(auv)
# save AUV as shapefile --
#writeOGR(auv, s.dir, "Auv_CMR", drive = "ESRI Shapefile")
#auv <- readOGR(paste(s.dir, "Auv_CMR.shp", sep='/'))
#plot(gb)
#plot(auv, add=T)


## read DTV sp ----
#dtv <- read.csv(paste(d.dir, "DTV_detailed_habitat_dominant.csv", sep='/'))
#str(dtv)
# make dtv sp points --
#coordinates(dtv) <- ~Longitude+Latitude
#proj4string(dtv) <- proj4string(gb)
#plot(gb)
#points(dtv)
#dtv <- crop(dtv, gb)
# save dtv as csv --
#dtv2 <- as.data.frame(dtv)
#str(dtv2)
#write.csv(dtv2, paste(d.dir, "Dtv_CMR.csv", sep='/'))
dtv <- read.csv(paste(d.dir, "Dtv_CMR.csv", sep='/'))
str(dtv)
# save dtv as shapefile --
#writeOGR(dtv, s.dir, "Dtv_CMR", drive = "ESRI Shapefile")
#dtv <- readOGR(paste(s.dir, "Dtv_CMR.shp", sep='/'))
#plot(gb)
#plot(dtv, add=T)


#### Plot  ####

# set colors #
# for color list: https://kbroman.files.wordpress.com/2014/05/crayons.png
grey <- brocolors("crayons")["Timberwolf"] # "#dbd7d2"
green <- brocolors("crayons")["Spring Green"] # "#eceabe"
blue <- brocolors("crayons")["Sky Blue"] # "#80daeb"
green2 <- brocolors("crayons")["Sea Green"] # "#93dfb8"
land <- brocolors("crayons")["Gold"] # "#deaa88"
ocean <- brocolors("crayons")["Blizzard Blue"]

## To plot the Geo Bay polygon ----
gb@data$id = rownames(gb@data)
gb.points = broom::tidy(gb)
gb.df = join(gb.points, gb@data, by="id")
class(gb.df)
str(gb.df)
levels(gb.df$ZoneName)
levels(gb.df$ZoneName)[levels(gb.df$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"

## To plot the coast polygon ----
coast@data$id = rownames(coast@data)
coast.points = broom::tidy(coast)
coast.df = join(coast.points, coast@data, by="id")
class(coast.df)
str(coast.df)

## plot ---

p0 <- ggplot() +
  geom_polygon(data = gb.df, aes(x = long, y = lat, group = group, fill = ZoneName), color = "black") +
  geom_polygon(data = coast.df, aes(x = long, y = lat, group = group), fill = land, color = "black") +
  #geom_point(aes(x=coords.x1, y=coords.x2), data=bruv) +
  #geom_path(color = "white", size = 0.2) +
  #scale_fill_gradient(breaks=c(0.33,0.66,0.99), labels=c("Low","Medium","High")) + 
  coord_equal(ratio= 1) +
  #xlab("Latitude") + ylab("Longitude") +
  scale_fill_manual("Zones", values = c("#eceabe", "#80daeb", "#93dfb8" , "#dbd7d2"), guide=guide_legend(nrow=4, title.position = 'top'))+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank()) +
  #theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_text(face="bold")) +
  #labs(title = "Geographe Bay") +
  xlab("Latitude") + ylab("Longitude") +
  #xlim(114.95, 115.65) + ylim( -33.7, -33.25 ) +
  #geom_point(aes(x=coords.x1, y=coords.x2, color = bruv$ZoneName), data=bruv,alpha=1, size=3, color="grey20")+ # to get outline
  #add the points from surveys --
  # BRUV --
  geom_point(aes(x=coords.x1, y=coords.x2, color = "Stereo-BRUVs"), data=bruv,alpha=1, size=1.5) +
  # AUV --
  geom_point(aes(x=coords.x1, y=coords.x2, color = "AUV grids"), data=auv,alpha=1, size=1.5) +
  # FTV --
  geom_point(aes(x=coords.x1, y=coords.x2, color = "Forward Towed Video transects"), data=ftv,alpha=1, size=1.5) +
  # DTV --
  geom_point(aes(x=Longitude, y=Latitude, color = "Downward towed Video transects"), data=dtv,alpha=1, size=1.5) +
  scale_colour_manual("Benthic survey methods", values = c("blue", "red","green3", "black"), guide=guide_legend(nrow=4, title.position = "top")) +  # change color scale
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_text(face="bold"),
        panel.background = element_rect(fill = "lightblue1"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotation_north_arrow(which_north = 'grid', height = unit(1.5, "cm"), width = unit(1.5, "cm"), pad_x = unit(15.25, "cm"),
                         pad_y = unit(0.59, "cm")) 
  #annotation_scale(plot_unit = 'km')
  #ggsn::scalebar(gb, location = "bottomright", dist = 2, st.size = 10, height = 0.05, transform = TRUE, dist_unit= 'km', model = 'GRS80', label = '10 km', 
  #               x.min = 115.0 , x.max = 115.6 , y.min = -33.69, y.max = -33.35)                                                                                                            
  #xlab("Latitude") + ylab("Longitude") 

p0

#ggsave("GB-All-surveys.png", plot = p0, path = p.dir, width = 200, height = 134, units = "mm", dpi = 300)

