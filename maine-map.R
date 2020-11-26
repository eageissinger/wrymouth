# ---- mapping in R ------

#load packages
## need to confirm which packages were used for the final maps.
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
library(ggpubr)
library(patchwork)


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

ME_UTM<-spTransform(ME, CRS("+init=EPSG:2802"))

ME_UTM@data$NAME_1

ggplot()+
  geom_polygon(data=ME_UTM,aes(long,lat))+
  geom_path(data=ME_UTM,aes(long,lat),colour='grey',size=0.1)

ggplot()+
  geom_polygon(data=ME,aes(long,lat))+
  geom_path(data=ME,aes(long,lat))

Lat.lim=c(44.33,44.83)
Long.lim=c(-67.8,-67.1)
map("worldHires",xlim=Long.lim,ylim=Lat.lim,col="grey",
    fill=TRUE, resolution=0);map.axes()
map.scale(ratio=FALSE)

plot(ME,xlim=Long.lim,ylim=Lat.lim,col='grey')

ggplot(ME)+
  geom_sp()

# add new column composed of rownames
ME@data$id<-rownames(ME@data)

#create a data.frame from the spactial object
MEpoints<-fortify(ME, region="id")
MEdf<-merge(MEpoints,ME@data,by="id")

head(MEdf)

ggplot(data=MEdf,aes(x=long,y=lat,group=group))+
  geom_polygon(fill='cornsilk')+
  geom_path(colour='black')+
  coord_equal()+
  theme(legend.position = "none",title=element_blank(),axis.text=element_blank())+
  geom_rect(xmin=-67.65,xmax=-67.26,ymin=44.45,ymax=44.75,
            fill=NA,colour='black',size=1.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background=element_rect(fill='gray95',
                                colour='gray95'))+
  xlab("Longitude")+
  ylab("Latitude")

maine<-ggplot(data=MEdf,aes(x=long,y=lat,group=group))+
  geom_polygon(fill='cornsilk')+
  geom_path(colour='black')+
  coord_equal()+
  theme(legend.position = "none",title=element_blank(),axis.text=element_blank())+
  geom_rect(xmin=-67.65,xmax=-67.26,ymin=44.45,ymax=44.75,
            fill=NA,colour='darkred',size=1.25)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background=element_rect(fill='gray95',
                                      colour='gray95'))+
  theme(axis.title = element_blank())+
  theme(axis.text = element_blank())+
  theme(axis.line=element_blank(),
        axis.ticks = element_blank(),plot.background = element_rect(fill="gray95"))+
  theme(plot.margin = margin(0,0,0,0,'pt'))

Lat.lim=c(44.45,44.75)
Long.lim=c(-67.65,-67.26)
coast<-ggplot(data=MEdf,aes(x=long,y=lat,group=group))+
  geom_polygon(fill='cornsilk')+
  geom_path(colour='black')+
  theme(legend.position = "none",title=element_blank(),axis.text=element_blank())+
  coord_fixed(xlim=Long.lim,ylim=Lat.lim)+
  geom_point(aes(x=-67.586598,y=44.48559),colour='white',shape=22,size=3,fill='darkred')+
  geom_point(aes(x=-67.387599,y=44.671753),colour='white',shape=24,size=3,fill='darkred')+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background=element_rect(fill='gray95',
                                      colour='gray95'))+
  theme(axis.title=element_blank())

Lat.lim.a=c(44.63,44.7)
Long.lim.a=c(-67.41,-67.34)
LAR<-ggplot(data=MEdf,aes(x=long,y=lat,group=group))+
  geom_polygon(fill='cornsilk')+
  geom_path(colour='black')+
  theme(legend.position = "none",title=element_blank(),axis.text=element_blank())+
  coord_fixed(xlim=Long.lim.a,ylim=Lat.lim.a)+
  geom_point(aes(x=-67.387599,y=44.671753),colour='white',shape=24,size=2.5,fill='darkred')+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background=element_rect(fill='gray95',
                                      colour='gray95'))+
  theme(axis.title = element_blank())

Lat.lim.b=c(44.44,44.51)
Long.lim.b=c(-67.62,-67.55)
MHC<-ggplot(data=MEdf,aes(x=long,y=lat,group=group))+
  geom_polygon(fill='cornsilk')+
  geom_path(colour='black')+
  theme(legend.position = "none",title=element_blank(),axis.text=element_blank())+
  coord_fixed(xlim=Long.lim.b,ylim=Lat.lim.b)+
  geom_point(aes(x=-67.586598,y=44.48559),colour='white',shape=22,size=2.5,fill='darkred')+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background=element_rect(fill='gray95',
                                      colour='gray95'))+
  theme(axis.title=element_blank())


