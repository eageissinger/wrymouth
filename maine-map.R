# ---- mapping in R ------

#load packages
library(tidyverse)
library(maptools)
library(raster)
library(maps)
library(mapproj)
library(mapdata)
library(rgeos)
library(sp)
library(rgdal)
library(ggmap)
library(marmap)
library(lattice)
library(sf)
library(robis)
library(gridExtra)
library(devtools)
library(ggsn)
library(broom)
library(tmap)
library(cowplot)
library(ggrepel)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)


# ---- Getting a basemap ----
data(wrld_simpl)
plot(wrld_simpl)
plot(wrld_simpl,col='olivedrab3',bg='lightblue')


# ---- exporting and importing ----
writeOGR(wrld_simpl,dsn=getwd(),layer="world_test",
         driver="ESRI Shapefile",overwrite_layer = TRUE)
world_shp<-readOGR(dsn=getwd(),layer="world_test")
plot(world_shp)

# ---- making nicer maps
# using raster
Canada<-getData('GADM',country="CAN",level=1)
plot(Canada)

Canada

US<-getData('GADM',country="USA",level=1) #never use level 2, it will cause computer to crash
plot(US)

ME<-US[US$NAME_1=="Maine",]
plot(ME)

Lat.lim=c(44.33,44.83)
Long.lim=c(-67.8,-67.1)
map("worldHires",xlim=Long.lim,ylim=Lat.lim,col="grey",
    fill=TRUE, resolution=0);map.axes()
map.scale(ratio=FALSE)

plot(ME,xlim=Long.lim,ylim=Lat.lim,col='grey')

ggplot(ME)+
  geom_sp()
