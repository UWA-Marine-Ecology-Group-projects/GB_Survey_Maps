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


### STREO-BRUVS ####
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
zonecolors <- c("#93dfb8" , "#eceabe", "#80daeb", "#dbd7d2")


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



## FTV ####
# ## read BRUV data ----
ftv <- read.csv(paste(d.dir, "towed_detailed.percent.cover_20200914.csv", sep='/'))
str(ftv)
head(ftv)
# create id column
id <- seq(1:2962)
ftv$id <- id
head(ftv)
# make sp --
tsp <- ftv
coordinates(tsp) <- ~longitude+latitude
proj4string(tsp) <- proj4string(gb)
plot(gb)
points(tsp)
tsp <- crop(tsp, gb)
tsp2 <- raster::extract(gb, tsp, df=TRUE)

# make df--
tsp <- as.data.frame(tsp)
tsp2 <- as.data.frame(tsp2)
tsp <- droplevels(tsp)
tsp2 <- droplevels(tsp2)
str(tsp) # 1141
str(tsp2) # 1142 
# remove duplicated row
tsp2$point.ID[duplicated(tsp2$point.ID)]
tsp2 <- tsp2[-1034,]
# join with rest
ftv <- cbind(tsp, tsp2)
str(ftv)
names(ftv)
# remove unnecessary columns --
ftv <- ftv[,c(1:10, 22)]
head(ftv)
ftv$unk.strap.like <- ftv$Strap.like.leaves-(ftv$Amphibolis+ftv$Posidonia+ftv$Thalassodendrum)
head(ftv)
levels(ftv$ZoneName)[levels(ftv$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"
head(ftv)


# get averages ----
# se function --
se <- function(x) sd(x)/sqrt(length(x))
# amphibolis --
amp <- aggregate(Amphibolis~ZoneName, data = ftv, mean)
ampse <- aggregate(Amphibolis~ZoneName, data = ftv, se)
# posidonia --
pos <- aggregate(Posidonia~ZoneName, data = ftv, mean)
pose <- aggregate(Posidonia~ZoneName, data = ftv, se)
# Zostera --
th <- aggregate(Thalassodendrum~ZoneName, data = ftv, mean)
thse <- aggregate(Thalassodendrum~ZoneName, data = ftv, se)
# unknown strap --
unk <- aggregate(unk.strap.like~ZoneName, data = ftv, mean)
unkse <- aggregate(unk.strap.like~ZoneName, data = ftv, se)

# join -- 
tsg <- cbind(amp, pos, th,  unk)
head(tsg )
names(tsg )
tsg  <- tsg [,c(1,2,4,6,8)]
head(tsg )

tsg1 <- cbind(ampse, pose, thse,  unkse)
head(tsg1)
names(tsg1)
tsg1 <- tsg1[,c(1,2,4,6,8)]
head(tsg1)
names(tsg1) <- c("ZoneName",  "AmphibolisSE","PosidoniaSE",  "ThalassoSE" ,    "unk.strap.like.SE")

# make long ---
tsg <- melt(tsg, od.vars=c("ZoneName"))
head(tsg)
names(tsg) <- c("ZoneName", "Seagrass", "Mean")

tsg1 <- melt(tsg1, od.vars=c("ZoneName"))
head(tsg1)
names(tsg1) <- c("ZoneName", "Seagrass", "SE")

tsg2 <- cbind(tsg, tsg1)
head(tsg2)
tsg2 <- tsg2[,c(1,2,3,6)]
head(tsg2)
str(tsg2)
# rename factors ----
levels(tsg2$Seagrass)[levels(tsg2$Seagrass)=="unk.strap.like"] <- "Unidentified strap-like"
levels(tsg2$ZoneName)[levels(tsg2$ZoneName)=="National Park Zone"] <- "NPZ"
levels(tsg2$ZoneName)[levels(tsg2$ZoneName)=="Habitat Protection Zone"] <- "HPZ"
levels(tsg2$ZoneName)[levels(tsg2$ZoneName)=="Multiple Use Zone"] <- "MUZ"
levels(tsg2$ZoneName)[levels(tsg2$ZoneName)=="Special Purpose Zone"] <- "SPZ"

# reorder factors for plotting ----
tsg2$ZoneName <- ordered(tsg2$ZoneName, levels=c("NPZ", "HPZ", "MUZ", "SPZ"))

# remove Thalassodendron
tsg2 <- tsg2[tsg2$Seagrass!="Thalassodendrum",]
levels(tsg2$Seagrass)
tsg2 <- droplevels(tsg2)
             
# plot FTV seagrass ----

# FTV colors ----
#bluepal <- choose_palette()
#greenpal <- choose_palette()
#zonecolors <- c("#93dfb8" , "#eceabe", "#80daeb", "#dbd7d2")


theme_set(theme_bw())
pf <-ggplot(data=tsg2, aes(x=ZoneName, y=Mean, fill=ZoneName)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_wrap(~Seagrass, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  #scale_fill_manual(values = greenpal(4)) +
  scale_fill_manual(values = zonecolors) +
  labs(title = "FTV", y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold"))

pf

ggsave("Seagrass.FTV.png", plot = pf, path = p.dir, scale=1, dpi = 300)



### AUV ####
# ## read AUV data ----
auv <- read.csv(paste(d.dir, "auv_detailed.percent.cover_20200914.csv", sep='/'))
str(auv)
head(auv)
# create id column
#id <- seq(1:2962)
#ftv$id <- id
#head(ftv)
# make sp --
asp <- auv
coordinates(asp) <- ~longitude+latitude
proj4string(asp) <- proj4string(gb)
plot(gb)
points(asp)
#asp <- crop(asp, gb)
asp2 <- raster::extract(gb, asp, df=TRUE)
str(asp2)
# make df--
asp <- as.data.frame(asp)
#tsp2 <- as.data.frame(tsp2)
#tsp <- droplevels(tsp)
#tsp2 <- droplevels(tsp2)
#str(tsp) # 1141
#str(tsp2) # 1142 
# remove duplicated row
#asp2$point.ID[duplicated(asp2$point.ID)]
#asp2 <- asp2[-1034,]
# join with rest
auv <- cbind(asp, asp2)
str(auv)
names(auv)
# remove unnecessary columns --
auv <- auv[,c(1:12, 26)]
head(auv)
auv$unk.strap.like <- auv$Strap.like.leaves-(auv$Amphibolis+auv$Posidonia+auv$Zostera+auv$Rupia)
head(auv)
levels(auv$ZoneName)[levels(auv$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"
head(auv)


# get averages ----
# se function --
se <- function(x) sd(x)/sqrt(length(x))
# amphibolis --
amp <- aggregate(Amphibolis~ZoneName, data = auv, mean)
ampse <- aggregate(Amphibolis~ZoneName, data = auv, se)
# posidonia --
pos <- aggregate(Posidonia~ZoneName, data = auv, mean)
pose <- aggregate(Posidonia~ZoneName, data = auv, se)
# Zostera --
zo <- aggregate(Zostera~ZoneName, data = auv, mean)
zose <- aggregate(Zostera~ZoneName, data = auv, se)
# Rupia --
r <- aggregate(Rupia~ZoneName, data = auv, mean)
rse <- aggregate(Rupia~ZoneName, data = auv, se)
# unknown strap --
unk <- aggregate(unk.strap.like~ZoneName, data = auv, mean)
unkse <- aggregate(unk.strap.like~ZoneName, data = auv, se)

# join -- 
asg <- cbind(amp, pos, zo, r, unk)
head(asg)
names(asg)
asg  <- asg [,c(1,2,4,6,8,10)]
head(asg )

asg1 <- cbind(ampse, pose, zose, rse,  unkse)
head(asg1)
names(asg1)
asg1<- asg1[,c(1,2,4,6,8, 10)]
head(asg1)
names(asg1) <- c("ZoneName",  "AmphibolisSE","PosidoniaSE",  "ZosteraSE" ,  "RupiaSE",  "unk.strap.like.SE")

# make long ---
asg <- melt(asg, od.vars=c("ZoneName"))
head(asg)
names(asg) <- c("ZoneName", "Seagrass", "Mean")

asg1 <- melt(asg1, od.vars=c("ZoneName"))
head(asg1)
names(asg1) <- c("ZoneName", "Seagrass", "SE")

asg2 <- cbind(asg, asg1)
head(asg2)
str(asg2)
asg2 <- asg2[,c(1,2,3,6)]
levels(asg2$Seagrass)
head(asg2)
str(asg2)
# rename factors ----
levels(asg2$Seagrass)[levels(asg2$Seagrass)=="unk.strap.like"] <- "Unidentified strap-like"
levels(asg2$ZoneName)[levels(asg2$ZoneName)=="National Park Zone"] <- "NPZ"
levels(asg2$ZoneName)[levels(asg2$ZoneName)=="Habitat Protection Zone"] <- "HPZ"
levels(asg2$ZoneName)[levels(asg2$ZoneName)=="Multiple Use Zone"] <- "MUZ"
levels(asg2$ZoneName)[levels(asg2$ZoneName)=="Special Purpose Zone"] <- "SPZ"

levels(asg2$Seagrass)
# reorder factors for plotting ----
asg2$ZoneName <- ordered(asg2$ZoneName, levels=c("NPZ", "HPZ", "MUZ", "SPZ"))

# remove levels
levels(asg2$Seagrass)
asg2 <- asg2[asg2$Seagrass!="Rupia",]
asg2 <- droplevels(asg2)

# plot FTV seagrass ----

# AuV colors ----
#bluepal <- choose_palette()
#greenpal <- choose_palette()
#zonecolors <- c("#93dfb8" , "#eceabe", "#80daeb", "#dbd7d2")


theme_set(theme_bw())
pa <-ggplot(data=asg2, aes(x=ZoneName, y=Mean, fill=ZoneName)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_wrap(~Seagrass, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  #scale_fill_manual(values = greenpal(4)) +
  scale_fill_manual(values = zonecolors) +
  labs(title = "AUV", y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold"))

pa

ggsave("Seagrass.AUV.png", plot = pa, path = p.dir, scale=1, dpi = 300)


## DTV ####
# ## read BRUV data ----
dtv <- read.csv(paste(d.dir, "DTV-habitat-percent-cover-detailed-06102020.csv", sep='/'))
str(dtv)
#dtv$Latitude <- dtv$Latitude*(-1)
head(dtv)
# create id column
#id <- seq(1:2962)
#dtv$id <- id
#head(dtv)
# make sp --
dsp <- dtv
coordinates(dsp) <- ~Longitude+Latitude
proj4string(dsp) <- proj4string(gb)
plot(gb)
points(dsp)
dsp <- crop(dsp, gb)
dsp2 <- raster::extract(gb, dsp, df=TRUE)

# make df--
dsp <- as.data.frame(dsp)
#tsp <- droplevels(tsp)
#tsp2 <- droplevels(tsp2)
str(dsp) # 4846
str(dsp2) # 4846
# remove duplicated row
#dsp2$point.ID[duplicated(dsp2$point.ID)]
#dsp2 <- dsp2[-1034,]
# join with rest
dtv <- cbind(dsp, dsp2)
str(dtv)
names(dtv)
# remove unnecessary columns --
dtv <- dtv[,c(2:9,17,18,21,29)]
head(dtv)

### UP TO HERE
dtv$unk.strap.like <- dtv$total.seagrass-
  (dtv$Amphibolis+dtv$Posidonia+
     dtv$Halophila)
head(dtv)
levels(dtv$ZoneName)[levels(dtv$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"
head(dtv)


# get averages ----
# se function --
se <- function(x) sd(x)/sqrt(length(x))
# amphibolis --
amp <- aggregate(Amphibolis~ZoneName, data = dtv, mean)
ampse <- aggregate(Amphibolis~ZoneName, data = dtv, se)
# posidonia --
pos <- aggregate(Posidonia~ZoneName, data = dtv, mean)
pose <- aggregate(Posidonia~ZoneName, data = dtv, se)
# Zostera --
h <- aggregate(Halophila~ZoneName, data = dtv, mean)
hse <- aggregate(Halophila~ZoneName, data = dtv, se)
# unknown strap --
unk <- aggregate(unk.strap.like~ZoneName, data = dtv, mean)
unkse <- aggregate(unk.strap.like~ZoneName, data = dtv, se)

# join -- 
dsg <- cbind(amp, pos, h,  unk)
head(dsg )
names(dsg)
dsg  <- dsg [,c(1,2,4,6,8)]
head(dsg )

dsg1 <- cbind(ampse, pose, hse,  unkse)
head(dsg1)
names(dsg1)
dsg1 <- dsg1[,c(1,2,4,6,8)]
head(dsg1)
names(dsg1) <- c("ZoneName",  "AmphibolisSE","PosidoniaSE",  "HalophilaSE" ,    "unk.strap.like.SE")

# make long ---
dsg <- melt(dsg, od.vars=c("ZoneName"))
head(dsg)
names(dsg) <- c("ZoneName", "Seagrass", "Mean")

dsg1 <- melt(dsg1, od.vars=c("ZoneName"))
head(dsg1)
names(dsg1) <- c("ZoneName", "Seagrass", "SE")

dsg2 <- cbind(dsg, dsg1)
head(dsg2)
dsg2 <- dsg2[,c(1,2,3,6)]
head(dsg2)
str(dsg2)
# rename factors ----
levels(dsg2$Seagrass)[levels(dsg2$Seagrass)=="unk.strap.like"] <- "Unidentified strap-like"
levels(dsg2$ZoneName)[levels(dsg2$ZoneName)=="National Park Zone"] <- "NPZ"
levels(dsg2$ZoneName)[levels(dsg2$ZoneName)=="Habitat Protection Zone"] <- "HPZ"
levels(dsg2$ZoneName)[levels(dsg2$ZoneName)=="Multiple Use Zone"] <- "MUZ"
levels(dsg2$ZoneName)[levels(dsg2$ZoneName)=="Special Purpose Zone"] <- "SPZ"

# reorder factors for plotting ----
dsg2$ZoneName <- ordered(dsg2$ZoneName, levels=c("NPZ", "HPZ", "MUZ", "SPZ"))

# plot FTV seagrass ----

# FTV colors ----
bluepal <- choose_palette()
greenpal <- choose_palette()
zonecolors <- c("#93dfb8" , "#eceabe", "#80daeb", "#dbd7d2")


theme_set(theme_bw())
pd <-ggplot(data=dsg2, aes(x=ZoneName, y=Mean, fill=ZoneName)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_wrap(~Seagrass, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  #scale_fill_manual(values = greenpal(4)) +
  scale_fill_manual(values = zonecolors) +
  labs(title = "DTV", y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold"))

pd

#ggsave("Seagrass.DTV.png", plot = pd, path = p.dir, scale=1, dpi = 300)


### All methods  -----

head(bsg2)
bsg2$Method <- "Stereo-BRUVs"
head(tsg2)
tsg2$Method <- "FTV"
head(asg2)
asg2$Method <- "AUV"
head(dsg2)
dsg2$Method <- "DTV"

# join 
all <- rbind(bsg2, tsg2, asg2, dsg2)
all

# save this data --
#write.csv(all, paste(d.dir, "all.seagrass.all.methods.csv", sep='/'))


## plot all ----
bluepal <- choose_palette()
greenpal2 <- choose_palette()

levels(all$Seagrass)
all <- all[all$Seagrass!="Zostera",]
all <- all[all$Seagrass!="Halophila",]

theme_set(theme_bw())
pall <-ggplot(data=all, aes(x=Method, y=Mean, fill=ZoneName)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_grid(ZoneName~Seagrass) +
  #facet_wrap(~Zone, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal2(5)) +
  #labs(title = "Autonomous Underwater Vehicle", y = "Mean % cover") +
  labs(y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text.x = element_text(size = 8, color = "black", face ="bold"),
        strip.text.y = element_text(size = 10, color = "black", face ="bold"))

pall


ggsave("Seagrass2.png", plot = pall, path = p.dir, scale=1, dpi = 300)
