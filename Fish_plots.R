
##      ##       ##      ##      ##      ##

## Script to do fish plots ----

# libraries ----
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
#install.packages("magritrr")
library(magritrr)

# clear workspace ----
rm(list = ls())

# set working directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "plots", sep='/')


# read maxn ----
mxn <- read.csv(paste(d.dir, "2014-12_Geographe.Bay_stereoBRUVs.checked.maxn.csv", sep='/'))
str(mxn)
mxn$full.name <- paste(mxn$genus, mxn$species, sep = ' ')
head(mxn)
str(mxn)
mxn$full.name <- as.factor(mxn$full.name)
str(mxn) # 158 Species
class(mxn)

# read metadata ---
md <- read.csv(paste(d.dir, "BRUVs_2014_CMR.csv", sep='/'))
head(md)
str(md)
# keep only wanted cols --
md <- md %>% dplyr::select(sample, ZoneName, coords.x1, coords.x2)
str(md)
class(md)

# merge fish data with zone name ----
f <- merge(mxn, md, by='sample', all.x = FALSE)
str(f)

## aggregate by zone ----
# mean ----
fmean <- aggregate(maxn ~ full.name + ZoneName, data = f, mean)
head(fmean)

mean.maxn <- fmean %>% 
   group_by(ZoneName) %>% # group by zone
   top_n(10) %>% # get largest 10 values
   ungroup # 

head(mean.maxn)  
str(mean.maxn)

# sum ----
fsum <- aggregate(maxn ~ full.name + ZoneName, data = f, sum)
head(fsum)

sum.maxn <- fsum %>% 
  group_by(ZoneName) %>% # group by zone
  top_n(10) %>% # get largest 10 values
  ungroup # 

head(sum.maxn)  
str(sum.maxn)


# PLOT ----
# Top 10 sp mean maxn per zone ----
levels(mean.maxn$ZoneName)


zonenames <- c("NPZ", "HPZ", "MUZ", "SPZ")

theme_set(theme_bw())
pd <-ggplot(data=mean.maxn, aes(x=full.name, y=maxn)) +
  geom_bar(stat="identity", color = "black") +
  #geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_wrap(~ZoneName, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  #scale_fill_manual(values = greenpal(4)) +
  #scale_fill_manual(values = zonecolors) +
  #labs(title = "DTV", y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold"))

pd
