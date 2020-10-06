####    Bar graph of % cover of benthic classes   ####


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
#devtools::install_github("3wen/legendMap")
library(ggsn)
library(grid)
library(maps)
library(broom)
library(tidyverse)
library(dplyr)
library(ggspatial)
library(colorspace)

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

###  BRUV PLOT ----

## read BRUV data ----
bruv <- read.csv(paste(d.dir, "BRUVs_2014_CMR.csv", sep='/'))
str(bruv)
head(bruv)

# Bruv to long format ----

bruvlong <- gather(bruv, Class, measurement, Macroalgae:Consolidated, factor_key =T)
head(bruvlong)
str(bruvlong)
levels(bruvlong$ZoneName)[levels(bruvlong$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"

bav <- aggregate(measurement~Class+ZoneName, data = bruvlong, mean)
# define SE function --
se <- function(x) sd(x)/sqrt(length(x))
bse <- aggregate(measurement~Class+ZoneName, data = bruvlong, se)
head(bse)

b <- cbind(bav, se = bse[,3])
head(b)
b$lower <- b$measurement-b$se
b$upper <- b$measurement+b$se
head(b)
str(b)

# reorder factors for plotting ----
b$ZoneName <- ordered(b$ZoneName, levels=c("National Park Zone", "Habitat Protection Zone", "Multiple Use Zone", "Special Purpose Zone"))
b$Class <- ordered(b$Class, levels = c("Seagrasses", "Unconsolidated", "Turf.algae", "Macroalgae", 
                                       "Consolidated", "Sponges", "Stony.corals"))


# Bruv colors ----
bluepal <- choose_palette()
greenpal <- choose_palette()

# BRUV plot ----
theme_set(theme_bw())
pb <-ggplot(data=b, aes(x=Class, y=measurement, fill=Class)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_wrap(~ZoneName, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal(7)) +
  labs(title = "Stereo-BRUVs", y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = 10, color = "black", face ="bold"))

pb




###  FTV PLOT ----
## read FTV sp ----
ftv <- read.csv(paste(d.dir, "Ftv_CMR.csv", sep='/'))
str(ftv)


# Ftv to long format ----

ftvlong <- gather(ftv, Class, measurement, Macroalgae:Consolidated, factor_key =T)
head(ftvlong)
str(ftvlong)
levels(ftvlong$ZoneName)[levels(ftvlong$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"

fav <- aggregate(measurement~Class+ZoneName, data = ftvlong, mean)
# define SE function --
se <- function(x) sd(x)/sqrt(length(x))
fse <- aggregate(measurement~Class+ZoneName, data = ftvlong, se)
head(fse)

f <- cbind(fav, se = fse[,3])
head(f)
f$lower <- f$measurement-f$se
f$upper <- f$measurement+f$se
head(f)
str(f)

# reorder factors for plotting ----
f$ZoneName <- ordered(f$ZoneName, levels=c("National Park Zone", "Habitat Protection Zone", "Multiple Use Zone", "Special Purpose Zone"))
f$Class <- ordered(f$Class, levels = c("Seagrasses", "Unconsolidated", "Turf.algae", "Macroalgae", 
                                       "Consolidated", "Sponges"))


# FTV plot ----
theme_set(theme_bw())
pf <-ggplot(data=f, aes(x=Class, y=measurement, fill=Class)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_wrap(~ZoneName, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal(7)) +
  labs(title = "Forward Towed Video", y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = 10, color = "black", face ="bold"))

pf







#### AUV PLOT----
## read AUV sp ----
auv <- read.csv(paste(d.dir, "Auv_zoning.csv", sep='/'))
str(auv)

# Auv to long format ----

along <- gather(auv, Class, measurement, Macroalgae:Consolidated, factor_key =T)
head(along)
str(along)
levels(along$ZoneName)[levels(along$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"

aav <- aggregate(measurement~Class+ZoneName, data = along, mean)
# define SE function --
se <- function(x) sd(x)/sqrt(length(x))
ase <- aggregate(measurement~Class+ZoneName, data = along, se)
head(ase)

au <- cbind(aav, se = ase[,3])
head(au)
levels(au$ZoneName)
str(au)

# reorder factors for plotting ----
au$ZoneName <- ordered(au$ZoneName, levels=c("National Park Zone", "Habitat Protection Zone", "Multiple Use Zone", "Special Purpose Zone"))
au$Class <- ordered(au$Class, levels = c("Seagrasses", "Unconsolidated", "Turf.algae", "Macroalgae", 
                                       "Consolidated", "Sponges", "Stony.corals"))


# AUV plot ----
theme_set(theme_bw())
pa <-ggplot(data=au, aes(x=Class, y=measurement, fill=Class)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_wrap(~ZoneName, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal(7)) +
  labs(title = "Autonomous Underwater Vehicle", y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = 10, color = "black", face ="bold"))

pa





#### DTV PLOT ----
## read DTV sp ----
dtv <- read.csv(paste(d.dir, "Dtv_CMR.csv", sep='/'))
str(dtv)

# Dtv to long format ----

dlong <- gather(dtv, Class, measurement, total.seagrass:Macroalgae, factor_key =T)
head(dlong)
str(dlong)
levels(dlong$Zone)[levels(dlong$Zone)=="NPZ"] <- "National Park Zone"
levels(dlong$Zone)[levels(dlong$Zone)=="HPZ"] <- "Habitat Protection Zone"
levels(dlong$Zone)[levels(dlong$Zone)=="MUZ"] <- "Multiple Use Zone"
levels(dlong$Zone)[levels(dlong$Zone)=="SPZ"] <- "Special Purpose Zone"

dav <- aggregate(measurement~Class+Zone, data = dlong, mean)
# define SE function --
se <- function(x) sd(x)/sqrt(length(x))
dse <- aggregate(measurement~Class+Zone, data = dlong, se)
head(dse)

d <- cbind(dav, se = dse[,3])
head(d)
levels(d$Zone)
str(d)

# reorder factors for plotting ----
d$Zone <- ordered(d$ZoneName, levels=c("National Park Zone", "Habitat Protection Zone", "Multiple Use Zone", "Special Purpose Zone"))
d$Class <- ordered(d$Class, levels = c("total.seagrass", "Unconsolidated", "Turf.algae", "Macroalgae", 
                                         "Consolidated", "total.sponges", "corals", "other.inverts"))


# DTV plot ----
theme_set(theme_bw())
pd <-ggplot(data=d, aes(x=Class, y=measurement, fill=Class)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_wrap(~Zone, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal(8)) +
  labs(title = "Autonomous Underwater Vehicle", y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = 10, color = "black", face ="bold"))

pd


## join all methods ----

d # dtv
d$Method <- "DTV"
names(d) <- c("Class", "ZoneName"    ,    "measurement", "se" , "Method")
head(d)
au # auv
au$Method <- "AUV"
head(au)
f # ftv
f <- f[,c(1:4)]
head(f)
f$Method <- "FTV"
#levels(f$ZoneName)[levels(f$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"
b # bruvs
head(b)
b <- b[,c(1:4)]
b$Method <- "Stereo-BRUVs"

all <- rbind(d,au,b,f)
head(all)
str(all)
all$Method <- as.factor(all$Method)

# match names
levels(all$Class)
levels(all$Class)[levels(all$Class)=="total.seagrass"] <- "Seagrasses"
levels(all$Class)[levels(all$Class)=="total.sponges"] <- "Sponges"
levels(all$Class)[levels(all$Class)=="corals"] <- "Stony corals"
levels(all$Class)[levels(all$Class)=="Stony.corals"] <- "Stony corals"
levels(all$Class)[levels(all$Class)=="Turf.algae"] <- "Turf algae"

# remove other 
all <- all[all$Class!="other.inverts",]
all <- droplevels(all)
levels(all$Class)

# change the zones
all$ZoneName
levels(all$ZoneName)[levels(all$ZoneName)=="Habitat Protection Zone"] <- "HPZ"
levels(all$ZoneName)[levels(all$ZoneName)=="Multiple Use Zone"] <- "MUZ"
levels(all$ZoneName)[levels(all$ZoneName)=="National Park Zone"] <- "NPZ"
levels(all$ZoneName)[levels(all$ZoneName)=="Special Purpose Zone"] <- "SPZ"

all$ZoneName <- ordered(all$ZoneName, levels=c("NPZ","HPZ", "MUZ", "SPZ"))

head(all)
str(all)
# save ----
#write.csv(all, paste(d.dir, "mean.class.cover.allmethods.csv", sep='/'))

## To start plotting LOAD ALL DATA HERE ----
all <- read.csv(paste(d.dir, "mean.class.cover.allmethods.csv", sep ='/'))

# reorder factors for plotting ----
all$Class <- ordered(all$Class, levels=c("Seagrasses", "Unconsolidated", "Turf algae", 
                                         "Macroalgae", "Consolidated", "Sponges","Stony corals"))
all$ZoneName <- ordered(all$ZoneName, levels=c("NPZ","HPZ", "MUZ", "SPZ"))
all$Method <- ordered(all$Method, levels = c("Stereo-BRUVs",  "AUV" , "FTV", "DTV"))

## color plot ---
allpal <- c("#93dfb8" ,"#eceabe", "#80daeb",  "#dbd7d2")

## plot all ----

theme_set(theme_bw())
pall <-ggplot(data=all, aes(x=Class, y=measurement, fill=ZoneName)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_grid(ZoneName~Method) +
  #facet_wrap(~Zone, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = allpal) +
  #labs(title = "Autonomous Underwater Vehicle", y = "Mean % cover") +
  labs(y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = 10, color = "black", face ="bold"))

pall


## plot all2 ----

theme_set(theme_bw())
pall2 <-ggplot(data=all, aes(x=Method, y=measurement, fill=ZoneName)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_grid(ZoneName~Class) +
  #facet_wrap(~Zone, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal(7)) +
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

pall2

## save ----
#ggsave("Cover.Method.Class.Zone.png", plot = pall2, path = p.dir, width = 200, height = 134, units = "mm", dpi = 300)
#ggsave("Cover.Method.Class.Zone.png", plot = pall2, path = p.dir, scale=1, dpi = 300)

