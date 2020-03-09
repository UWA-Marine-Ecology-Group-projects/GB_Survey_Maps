### Map of the GB suvey of 2015 ####

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



## Read shape files from each zone (old zoning) -----

# Use library sf, to join geometries

MPw <- readOGR(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_MNPZ_West.shp", sep='/'))
#MPw <- st_read(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_MNPZ_West.shp", sep='/'))
plot(MPw)
str(MPw)
proj4string(MPw) # "+proj=utm +zone=50 +south +ellps=GRS80 +units=m +no_defs"

MPe <- readOGR(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_MNPZ_East.shp", sep='/'))
#MPe <- st_read(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_MNPZ_East.shp", sep='/'))
plot(MPe)
str(MPe)
proj4string(MPe)

SPZ <- readOGR(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_SPZ.shp", sep='/'))
#SPZ <- st_read(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_SPZ.shp", sep='/'))
plot(SPZ)
proj4string(SPZ)

MUZ <- readOGR(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_MUZ.shp", sep='/'))
#MUZ <- st_read(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_MUZ.shp", sep='/'))
plot(MUZ )
proj4string(MUZ )

plot(MUZ)
plot(MPe, add = T)
plot(MPw, add = T)
plot(SPZ, add = T)

## Read shp new GB zoning ----
gb <- readOGR(paste(w.dir, "shapefiles", "GeoBay.shp", sep='/'))
plot(gb)
proj4string(gb) # "+proj=longlat +ellps=GRS80 +no_defs"


## Read polygons of depth strata used for the balanced design ----
d1 <- readOGR(paste(w.dir, "shapefiles", "Depth strata", "CMR_Depth1A.shp", sep='/'))
#SPZ <- st_read(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_SPZ.shp", sep='/'))
plot(d1)
proj4string(d1)
d1

d2 <- readOGR(paste(w.dir, "shapefiles", "Depth strata", "CMR_Depth2A.shp", sep='/'))
#SPZ <- st_read(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_SPZ.shp", sep='/'))
plot(d2)
proj4string(d2)
d2

d3 <- readOGR(paste(w.dir, "shapefiles", "Depth strata", "CMR_Depth3A.shp", sep='/'))
#SPZ <- st_read(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_SPZ.shp", sep='/'))
plot(d3)
proj4string(d3)
d3

d4 <- readOGR(paste(w.dir, "shapefiles", "Depth strata", "CMR_Depth4A.shp", sep='/'))
#SPZ <- st_read(paste(w.dir, "shapefiles", "Geog_CMR_zones_UTM", "Geog_SPZ.shp", sep='/'))
plot(d4)
proj4string(d4)
d4


## Read Bruv data - this is already spatial points ----

# Points originally planned for CMR zones 
sp <- readOGR(paste(w.dir, "shapefiles", "XYNERP_points.shp", sep='/'))
str(sp) # 150 points
proj4string(sp) # "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
points(sp, pch = 21, bg = sp$Zone)

# Additional points for CMR and state waters
sp2 <- readOGR(paste(w.dir, "shapefiles", "XYSampleSites.shp", sep='/'))
str(sp2) # 300 points
proj4string(sp2) # "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
points(sp2, pch = 21, bg = sp2$Zone)

# Make Bruv points into data frame for plotting --
spdf <- as.data.frame(sp)
head(spdf)

spdf2 <- as.data.frame(sp2)
head(spdf2)


## To plot the Geo Bay polygon ---
gb@data$id = rownames(gb@data)
gb.points = broom::tidy(gb)
gb.df = join(gb.points, gb@data, by="id")
class(gb.df)
head(gb.df)

## To plot the the old national park zone East ---
MPe@data$id = rownames(MPe@data)
MPe.points = broom::tidy(MPe)
MPe.df = join(MPe.points, MPe@data, by="id")
class(MPe.df)
head(MPe.df)


## plot ----

# for color list: https://kbroman.files.wordpress.com/2014/05/crayons.png
grey <- brocolors("crayons")["Timberwolf"] # "#dbd7d2"
green <- brocolors("crayons")["Spring Green"] # "#eceabe"
blue <- brocolors("crayons")["Sky Blue"] # "#80daeb"
green2 <- brocolors("crayons")["Sea Green"] # "#93dfb8"
yellow <- brocolors("crayons")["Canary"] # "#ffff99"
orange <- brocolors("crayons")["Peach"] # "#ffcfab"

# Plot Original Bruvs for CMR with CMR zones ----

p <- ggplot() +
  geom_polygon(data = gb.df, aes(x = long, y = lat, group = group, fill = ZoneName), color = "black") +
  #geom_point(aes(x=coords.x1, y=coords.x2), data=sp) +
  #geom_path(color = "white", size = 0.2) +
  #scale_fill_gradient(breaks=c(0.33,0.66,0.99), labels=c("Low","Medium","High")) + 
  coord_equal(ratio= 1) +
  #xlab("Latitude") + ylab("Longitude") +
  scale_fill_manual("Zones", values = c( "#80daeb","#ffff99", "#93dfb8", "#ffcfab"), guide=guide_legend(nrow=4, title.position = 'top'))+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank()) +
  #theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_text(face="bold")) +
  labs(title = "Geographe Bay - 2015 CMR Stereo-BRUVS", x ="Latitude" , y ="Longitude") +
  xlab("Latitude") + ylab("Longitude") +
  geom_point(aes(x=coords.x1, y=coords.x2, color = Zone), data=spdf,alpha=1, size=3, color="grey20")+ # to get outline
  geom_point(aes(x=coords.x1, y=coords.x2, color = Zone), data=spdf,alpha=1, size=2) +
  scale_colour_manual("Stereo-BRUVs", values = c("blue", "red","green",  "yellow"), guide=guide_legend(nrow=3, title.position = "top")) +  # change color scale
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_text(face="bold")) +
  xlab("Latitude") + ylab("Longitude") 

print(p)


ggsave(paste(p.dir, "GB2015_Bruvs_CMR_original.png", sep='/'), plot=p, scale =1, device = "png", dpi =300)


# Plot Original and added Bruvs for CMR and state waters with CMR zones ----

p1 <- ggplot() +
  geom_polygon(data = gb.df, aes(x = long, y = lat, group = group, fill = ZoneName), color = "black") +
  #geom_point(aes(x=coords.x1, y=coords.x2), data=sp) +
  #geom_path(color = "white", size = 0.2) +
  #scale_fill_gradient(breaks=c(0.33,0.66,0.99), labels=c("Low","Medium","High")) + 
  coord_equal(ratio= 1) +
  #xlab("Latitude") + ylab("Longitude") +
  scale_fill_manual("Zones", values = c( "#80daeb","#ffff99", "#93dfb8", "#ffcfab"), guide=guide_legend(nrow=4, title.position = 'top'))+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank()) +
  #theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_text(face="bold")) +
  labs(title = "Geographe Bay - 2015 CMR Stereo-BRUVS", x ="Latitude" , y ="Longitude") +
  xlab("Latitude") + ylab("Longitude") +
  geom_point(aes(x=coords.x1, y=coords.x2, color = Zone), data=spdf2,alpha=1, size=3, color="grey20")+ # to get outline
  geom_point(aes(x=coords.x1, y=coords.x2, color = Zone), data=spdf2,alpha=1, size=2) +
  scale_colour_manual("Stereo-BRUVs", values = c("blue", "red","green",  "yellow"), guide=guide_legend(nrow=3, title.position = "top")) +  # change color scale
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_text(face="bold")) +
  xlab("Latitude") + ylab("Longitude") 

print(p1)


ggsave(paste(p.dir, "GB2015_Bruvs_CMR_originalnExtra.png", sep='/'), plot=p1, scale =1, device = "png", dpi =300)


