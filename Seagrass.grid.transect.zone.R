### Percent cover of seagrass at each grid or transect ###

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
#devtools::install_github("3wen/legendMap")
library(ggsn)
library(grid)
library(maps)
library(broom)
library(tidyverse)
library(dplyr)
library(ggspatial)
library(colorspace)
library(reshape2)

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

# ## read BRUV data ----
bruv <- read.csv(paste(d.dir, "stereo-BRUVs_detailed.percent.cover_20200914.csv", sep='/'))
str(bruv)
head(bruv)
# make sp --
bsp <- bruv
coordinates(bsp) <- ~longitude+latitude
proj4string(bsp) <- proj4string(gb)
plot(gb)
points(bsp)
bsp <- crop(bsp, gb)
bsp2 <- raster::extract(gb, bsp)
# make df--
bsp <- as.data.frame(bsp)
bsp2 <- as.data.frame(bsp2)
# join with rest
bruv <- cbind(bsp, bsp2)
str(bruv)
names(bruv)
# remove unnecessary columns --
bruv <- bruv[,c(1:11, 26)]
head(bruv)
bruv$unk.strap.like <- bruv$Strap.like.leaves-(bruv$Amphibolis+bruv$Posidonia+bruv$Zostera)
head(bruv)
levels(bruv$ZoneName)[levels(bruv$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"
head(bruv)

# get averages ----
# se function --
se <- function(x) sd(x)/sqrt(length(x))
# amphibolis --
amp <- aggregate(Amphibolis~ZoneName, data = bruv, mean)
ampse <- aggregate(Amphibolis~ZoneName, data = bruv, se)
# posidonia --
pos <- aggregate(Posidonia~ZoneName, data = bruv, mean)
pose <- aggregate(Posidonia~ZoneName, data = bruv, se)
# Zostera --
zos <- aggregate(Zostera~ZoneName, data = bruv, mean)
zose <- aggregate(Zostera~ZoneName, data = bruv, se)
# unknown strap --
unk <- aggregate(unk.strap.like~ZoneName, data = bruv, mean)
unkse <- aggregate(unk.strap.like~ZoneName, data = bruv, se)

# join -- 
bsg <- cbind(amp, pos, zos,  unk)
head(bsg)
names(bsg)
bsg <- bsg[,c(1,2,4,6,8)]
head(bsg)

bsg1 <- cbind(ampse, pose, zose,  unkse)
head(bsg1)
names(bsg1)
bsg1 <- bsg1[,c(1,2,4,6,8)]
head(bsg1)
names(bsg1) <- c("ZoneName",  "AmphibolisSE","PosidoniaSE",  "ZosteraSE" ,    "unk.strap.like.SE")
              

# make long ---
bsg <- melt(bsg, od.vars=c("ZoneName"))
head(bsg)
names(bsg) <- c("ZoneName", "Seagrass", "Mean")

bsg1 <- melt(bsg1, od.vars=c("ZoneName"))
head(bsg1)
names(bsg1) <- c("ZoneName", "Seagrass", "SE")

bsg2 <- cbind(bsg, bsg1)
head(bsg2)
bsg2 <- bsg2[,c(1,2,3,6)]
head(bsg2)
str(bsg2)
# rename factors ----
levels(bsg2$Seagrass)[levels(bsg2$Seagrass)=="unk.strap.like"] <- "Unidentified strap-like"
levels(bsg2$ZoneName)[levels(bsg2$ZoneName)=="National Park Zone"] <- "NPZ"
levels(bsg2$ZoneName)[levels(bsg2$ZoneName)=="Habitat Protection Zone"] <- "HPZ"
levels(bsg2$ZoneName)[levels(bsg2$ZoneName)=="Multiple Use Zone"] <- "MUZ"
levels(bsg2$ZoneName)[levels(bsg2$ZoneName)=="Special Purpose Zone"] <- "SPZ"

# reorder factors for plotting ----
bsg2$ZoneName <- ordered(bsg2$ZoneName, levels=c("NPZ", "HPZ", "MUZ", "SPZ"))


# plot bruv seagrass ----

# Bruv colors ----
bluepal <- choose_palette()
greenpal <- choose_palette()
zonecolors <- c("#eceabe", "#80daeb", "#93dfb8" , "#dbd7d2")


theme_set(theme_bw())
pb <-ggplot(data=bsg2, aes(x=ZoneName, y=Mean, fill=ZoneName)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_wrap(~Seagrass, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  #scale_fill_manual(values = greenpal(4)) +
  scale_fill_manual(values = zonecolors) +
  labs(title = "Stereo-BRUVs", y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold"))

pb

ggsave("Seagrass.BRUVs.png", plot = pb, path = p.dir, scale=1, dpi = 300)
