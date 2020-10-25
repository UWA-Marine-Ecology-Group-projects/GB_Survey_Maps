
##      ##       ##      ##      ##      ##

## Script to do fish plots ----

# libraries ----
#install.packages("ggplot2")
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
library(RColorBrewer)
#install.packages("magritrr")
library(magritrr)
#install.packages("tidytext")
library(tidytext)
#install.packages("ggtextures")
library(ggtextures) # to add images to ggplot
#install.packages("magick")
library(magick) # to add images to ggplot

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

se <-function(x) sd(x)/sqrt(length(x))
fse <- aggregate(maxn ~ full.name + ZoneName, data = f, se)
head(fse)

f2 <- cbind(fmean, se=fse[,3])
head(f2)


mean.maxn <- f2 %>% 
  group_by(ZoneName) %>% # group by zone
  top_n(10) %>% # get largest 10 values
  arrange(desc(maxn)) %>%
  arrange(desc(ZoneName)) %>% # arrange from largest to smallest
  ungroup 


#mean.maxn <- fmean %>% 
#   group_by(ZoneName) %>% # group by zone
#   top_n(10) %>% # get largest 10 values
 #   arrange(desc(maxn)) %>%
  # arrange(desc(ZoneName)) %>% # arrange from largest to smallest
  #ungroup 

head(mean.maxn)  
str(mean.maxn)

# sum ----
fsum <- aggregate(maxn ~ full.name + ZoneName, data = f, sum)
head(fsum)

sum.maxn <- fsum %>% 
  group_by(ZoneName) %>% # group by zone
  top_n(10) %>% # get largest 10 values
  arrange(desc(maxn))  %>% 
  ungroup # 

head(sum.maxn)  
str(sum.maxn)


# PLOT ----



### Plot TOP 10 species in desceding order of maxn faceted by zone ####

# https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets

dfmxn <- f2 %>% 
  group_by(ZoneName) %>% # group by zone
  top_n(10) %>% # get largest 10 values
  arrange(desc(maxn)) %>%
  arrange(desc(ZoneName)) %>% 
  # 1. Remove grouping
  ungroup() %>%
  # 2. Arrange by
  #   i.  facet group
  #   ii. bar height
  arrange(ZoneName, maxn) %>%
  # 3. Add order column of row numbers
  mutate(order = row_number())

head(dfmxn)
str(dfmxn)
dfmxn <- droplevels(dfmxn)

# rename zones
library(plyr)
dfmxn$ZoneName <- revalue(dfmxn$ZoneName, c("Habitat Protection Zone"="HPZ", "Multiple Use Zone"="MUZ", 
                                                    "National Park Zone"= "NPZ", "Special Purpose Zone (Mining Exclusion)" = "SPZ"))


levels(dfmxn$ZoneName)

# reorder the levels of zone
dfmxn$ZoneName <- ordered(dfmxn$ZoneName, levels = c("NPZ", "HPZ", "MUZ", "SPZ"))
levels(dfmxn$ZoneName)

head(dfmxn)

## PLOT ----

theme_set(theme_bw())

pd <-ggplot(data=dfmxn, aes(order, maxn), y=maxn) +
  geom_bar(stat="identity", color = "black", aes(fill = ZoneName)) +
  geom_errorbar(aes(ymin = maxn-se, ymax = maxn+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_wrap(~ZoneName, ncol = 2, scales = 'free') +
  scale_fill_manual(values = c("#93dfb8" ,"#eceabe", "#80daeb",  "#dbd7d2")) +
  #scale_fill_manual(values = greenpal(4)) +
  #scale_fill_manual(values = zonecolors) +
  labs(x = "Species", y = "mean MaxN") +
  # Add categories to axis
  scale_x_continuous(
    breaks = dfmxn$order,
    labels = dfmxn$full.name,
    expand = c(0,0)
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_text(size = 14, face="bold"), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 10), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(color = 'black', fill = "white"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold")) +
  coord_flip()

pd


#ggsave("GB-top10-fish.png", plot = pd, path = p.dir, width = 200, height = 134, units = "mm", dpi = 300)
ggsave("GB-top10-fish.png", plot = pd, path = p.dir, scale=1, dpi = 300)



## King wrasse ----
# read fish data --
d <- read.csv(paste(d.dir, "2014-12_Geographe.Bay_stereoBRUVs.king.wrasse.maxn.csv", sep='/'))
head(d)

# read metadata ---
md <- read.csv(paste(d.dir, "BRUVs_2014_CMR.csv", sep='/'))
head(md)
str(md)
# keep only wanted cols --
md <- md %>% dplyr::select(sample, ZoneName, coords.x1, coords.x2)
str(md)
class(md)

# merge fish data with zone name ----
f <- merge(d, md, by='sample', all.x = FALSE)
str(f)

## get means and se ----
fmean <- aggregate(maxn ~ZoneName, data = f, mean)

fse <- aggregate(maxn ~ZoneName, data = f, se)

f2 <- cbind(fmean, se = fse[,2])
head(f2)

f2$class <- 'King wrasse'
head(f2)
kw <- f2
head(kw)

######################

## Pink snapper ----
# read fish data --
d <- read.csv(paste(d.dir, "2014-12_Geographe.Bay_stereoBRUVs.pink.snapper.maxn.csv", sep='/'))
head(d)

# read metadata ---
md <- read.csv(paste(d.dir, "BRUVs_2014_CMR.csv", sep='/'))
head(md)
str(md)
# keep only wanted cols --
md <- md %>% dplyr::select(sample, ZoneName, coords.x1, coords.x2)
str(md)
class(md)

# merge fish data with zone name ----
f <- merge(d, md, by='sample', all.x = FALSE)
str(f)

## get means and se ----
fmean <- aggregate(maxn ~ZoneName, data = f, mean)

fse <- aggregate(maxn ~ZoneName, data = f, se)

f2 <- cbind(fmean, se = fse[,2])
head(f2)

f2$class <- 'Pink snapper'
head(f2)
ps <- f2
head(ps)


######################

## Species richness ----
# read fish data --
d <- read.csv(paste(d.dir, "GB_fish_cluster.csv", sep='/'))
head(d)
str(d)

# read metadata ---
md <- read.csv(paste(d.dir, "BRUVs_2014_CMR.csv", sep='/'))
head(md)
str(md)
# keep only wanted cols --
md <- md %>% dplyr::select(sample, ZoneName, coords.x1, coords.x2)
str(md)
class(md)
names(md) <- c("Sample"   , "ZoneName","coords.x1" ,"coords.x2")

# merge fish data with zone name ----
f <- merge(d, md, by='Sample', all.x = FALSE)
str(f)

# species counts to species richness --

fz <- f %>%
  dplyr::mutate(Richness = apply(.[3:160] > 0, 1, sum)) %>%
  dplyr::select(Sample, ZoneName, Richness)

head(fz)
str(fz)


## get means and se ----
fmean <- aggregate(Richness ~ZoneName, data = fz, mean)

fse <- aggregate(Richness ~ZoneName, data = fz, se)

f2 <- cbind(fmean, se = fse[,2])
head(f2)

f2$class <- 'Species richness'
head(f2)
sr <- f2
head(sr)


######################

## Legal sized fish  ----
# read fish data --
d <- read.csv(paste(d.dir, "2014-12_Geographe.Bay_stereoBRUVs.legal.sized.fish.csv", sep='/'))
head(d)

# read metadata ---
md <- read.csv(paste(d.dir, "BRUVs_2014_CMR.csv", sep='/'))
head(md)
str(md)
# keep only wanted cols --
md <- md %>% dplyr::select(sample, ZoneName, coords.x1, coords.x2)
str(md)
class(md)

# merge fish data with zone name ----
f <- merge(d, md, by='sample', all.x = FALSE)
str(f)
head(f)

# remove non legal --
f <- f[f$legal!="non-legal",]
f <- droplevels(f)
str(f)

## get means and se ----
fmean <- aggregate(number ~ZoneName, data = f, mean)

fse <- aggregate(number ~ZoneName, data = f, se)

f2 <- cbind(fmean, se = fse[,2])
head(f2)

f2$class <- 'Legal sized fish'
head(f2)
ls <- f2
head(ls)


######################

## Biomass > 20 cm  ----
# read fish data --
d <- read.csv(paste(d.dir, "2014-12_Geographe.Bay_stereoBRUVs.mass.summaries.csv", sep='/'))
head(d)

# read metadata ---
md <- read.csv(paste(d.dir, "BRUVs_2014_CMR.csv", sep='/'))
head(md)
str(md)
# keep only wanted cols --
md <- md %>% dplyr::select(sample, ZoneName, coords.x1, coords.x2)
str(md)
class(md)

# merge fish data with zone name ----
f <- merge(d, md, by='sample', all.x = FALSE)
str(f)

## get means and se ----
fmean <- aggregate(mass.20cm ~ZoneName, data = f, mean)

fse <- aggregate(mass.20cm ~ZoneName, data = f, se)

f2 <- cbind(fmean, se = fse[,2])
head(f2)

f2$class <- 'Biomass.20cm'
head(f2)
b20 <- f2
head(b20)


######################

## Biomass > 30 cm  ----
# read fish data --
d <- read.csv(paste(d.dir, "2014-12_Geographe.Bay_stereoBRUVs.mass.summaries.csv", sep='/'))
head(d)

# read metadata ---
md <- read.csv(paste(d.dir, "BRUVs_2014_CMR.csv", sep='/'))
head(md)
str(md)
# keep only wanted cols --
md <- md %>% dplyr::select(sample, ZoneName, coords.x1, coords.x2)
str(md)
class(md)

# merge fish data with zone name ----
f <- merge(d, md, by='sample', all.x = FALSE)
str(f)

## get means and se ----
fmean <- aggregate(mass.30cm ~ZoneName, data = f, mean)

fse <- aggregate(mass.30cm ~ZoneName, data = f, se)

f2 <- cbind(fmean, se = fse[,2])
head(f2)

f2$class <- 'Biomass.30cm'
head(f2)
b30 <- f2
head(b30)



## JOIN ALL FISH METRICS ----
names(kw)  <- c("ZoneName", "value" ,    "se"   ,    "class" )
names(ps) <- c("ZoneName", "value" ,    "se"   ,    "class" )
names(sr) <- c("ZoneName", "value" ,    "se"   ,    "class" )
names(ls) <- c("ZoneName", "value" ,    "se"   ,    "class" )
names(b20) <- c("ZoneName", "value" ,    "se"   ,    "class" )
names(b30) <- c("ZoneName", "value" ,    "se"   ,    "class" )

fall <- rbind(kw, ps, sr, ls, b20, b30)
str(fall)
fall$class <- as.factor(fall$class)
levels(fall$ZoneName)


# rename zones
library(plyr)
fall$ZoneName <- revalue(fall$ZoneName, c("Habitat Protection Zone"="HPZ", "Multiple Use Zone"="MUZ", 
                                            "National Park Zone"= "NPZ", "Special Purpose Zone (Mining Exclusion)" = "SPZ"))


levels(fall$ZoneName)

# reorder the levels of zone
fall$ZoneName <- ordered(fall$ZoneName, levels = c("NPZ", "HPZ", "MUZ", "SPZ"))
levels(fall$ZoneName)

# reorder class levels
levels(fall$class)
fall$class <- ordered(fall$class, levels = c("Species richness", "Legal sized fish", "King wrasse" , "Pink snapper",
                                                "Biomass.20cm", "Biomass.30cm"))
levels(fall$class)

# renames classes 
#fall$class <- revalue(fall$class, c("Species richness"="Species richness", "Legal sized fish"="Legal sized fish",
 #                                   "King wrasse"="King wrasse", "Pink snapper"="Pink snapper",  
  #                                  "Biomass.20cm"="Biomass of fish > 20 cm", "Biomass.30cm"="Biomass of fish > 30 cm"))
                         
                         
head(fall)


## PLOT ####
theme_set(theme_bw())

pd <-ggplot(data=fall, aes(x = ZoneName, y=value, fill = ZoneName)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = value-se, ymax = value+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_wrap(~class, ncol = 2, scales = 'free', strip.position = "left",
             labeller = as_labeller(c("Species richness"="Species richness", "Legal sized fish"="Abundance legal sized fish",
                                       "King wrasse"="Abundance King wrasse", "Pink snapper"="Abundance Pink snapper",  
                                       "Biomass.20cm"="Biomass of fish > 20 cm", "Biomass.30cm"="Biomass of fish > 30 cm"))) +
  scale_fill_manual(values = c("#93dfb8" ,"#eceabe", "#80daeb",  "#dbd7d2")) +
  #scale_fill_manual(values = greenpal(4)) +
  #scale_fill_manual(values = zonecolors) +
  #labs(x = "Zone") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_blank(), strip.placement = 'outside',
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 10, color = "black", face ="bold")) 


pd


#ggsave("GB-fish-metrics.png", plot = pd, path = p.dir, width = 200, height = 134, units = "mm", dpi = 300)
ggsave("GB-fish-metrics.png", plot = pd, path = p.dir, scale = 1 , dpi = 300)







## Add Fish pictures ---- 
## Have not managed to make this work

# need ggtextures package for this

# https://stackoverflow.com/questions/58677247/how-to-insert-pictures-into-each-individual-bar-in-a-ggplot-graph


fdata <- tibble(
  full.name = levels(dfmxn$full.name),
  image = list(
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Acanthaluteres vittiger-3cm.jpg"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Chromis westaustralis-3cmR.jpg"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Coris auricularis-3cmL.jpg"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Neatypus obliquus-3cmL.jpg"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Notolabrus parilus-3cm.jpg"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Opthalmolepis lineolatus-3cm.jpg"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/NoPicture.png"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Pempheris klunzingeri-3cmL.jpg"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Platycephalus speculator-3cm.jpg"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Platycephalus speculator-3cm.jpg"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Pseudocaranx dentex-3cm.jpg"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Sillaginodes_punctatus_nb_TAYLOR.png"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Sillago_bassensis_nb_TAYLOR.png"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Sillago_bassensis_nb_TAYLOR.png"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/NoPicture.png"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Trachinops_noarlungae_nb_TAYLOR.png"),
    image_read("G:/My Drive/meg_graphics/Graphics_fish pictures/Trachurus_novaezelandiae_nb_TAYLOR.png")
  )
)

dfish <- merge(dfmxn, fdata, by ='full.name')
head(dfish)
str(dfish)



pd <-ggplot(data=dfish, aes(order, maxn), y=maxn) +
  geom_bar(stat="identity", color = "black", aes(fill = ZoneName, image = image)) +
  #geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_wrap(~ZoneName, ncol = 2, scales = 'free') +
  scale_fill_manual(values = c("#93dfb8" ,"#eceabe", "#80daeb",  "#dbd7d2")) +
  geom_isotype_col(
    img_height = grid::unit(1, "null"), img_width = NULL,
    ncol = 1, nrow = 1, hjust = 1, vjust = 0.5
  ) +
  #scale_fill_manual(values = greenpal(4)) +
  #scale_fill_manual(values = zonecolors) +
  #labs(title = "DTV", y = "Mean % cover") +
  # Add categories to axis
  scale_x_continuous(
    breaks = dfmxn$order,
    labels = dfmxn$full.name,
    expand = c(0,0)
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold")) +
  coord_flip()

pd



















###     ##    ###     ##      ### 
## Another way, but not as good ----
# https://www.programmingwithr.com/how-to-reorder-arrange-bars-with-in-each-facet-of-ggplot/

pd <-ggplot(data=mean.maxn, aes(x=reorder_within(full.name, maxn, ZoneName), y=maxn)) +
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
        strip.text.y = element_text(size = 14, color = "black", face ="bold")) +
  coord_flip()

pd
