#work with bioclim data created previously

#load libraries
library(sp)
library(rgdal)
library(ggplot2)
library(RODBC)
library(reshape)
library(raster)
library(rgeos)  
library(spdep)


#get bioclim data on days in growing season and temp during growing season

bio_trop<-read.csv("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Climate data/Trop_bioclim2.csv",sep=",")
head(bio_trop)

#spatial join of site and climatic data

#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)

#import locations
Locations<- sqlFetch(sec, "Site characteristics")
Locations2<-subset(Locations,Latitude>-100)
head(Locations2)
colnames(Locations2)<-c("ID","Study","Site","Disturbance","Age","Type","Ref_Type","Ref_age","Country","Lat","Long","Temp","Rain","Elevation","Soil","Class")
Locations2<-subset(Locations2,Locations2$Disturbance!="Fire")
Locations2<-subset(Locations2,Locations2$Disturbance!="Logging")
Locations2<-subset(Locations2,Locations2$Disturbance!="Agroforestry")

#convert growing days data to spatial grid
str(bio_trop)

# Construct a SpatialGridDataFrame from climate data
head(bio_trop)
bio_trop<-subset(bio_trop,bio_trop$x>-180&bio_trop$x<180)
bio_trop<-subset(bio_trop,bio_trop$y>-90&bio_trop$y<90)
coordinates(bio_trop)<-~x+y
gridded(bio_trop)<-T

#bin growing days data becuase of doubt about exact site locations
bio_trop$roundy<-round(bio_trop$y*2)/2
bio_trop$roundx<-round(bio_trop$x*2)/2
roundxy<-data.frame(bio_trop$roundy,bio_trop$roundx)
head(bio_trop)
?tapply
test3<-tapply(bio_trop$days,roundxy,mean,simplify=T)
days_binned<-melt(test3)
days_binned<-days_binned[complete.cases(days_binned),]
colnames(days_binned)<-c("y","x","days")
#plot of number of days growing season in year
a<-ggplot(days_binned,aes(y=y,x=x,fill=days))+borders("world", size=0.1,colour="grey",fill="lightgrey")+geom_raster(alpha=0.5)+theme_bw()+theme(panel.grid.major = element_line(colour =NA))+theme_bw()+theme(panel.grid.major = element_line(colour =NA))
a
b<-a+coord_cartesian(xlim = c(-130, 180),ylim=c(-30, 30))
b+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+opts(legend.position="none")+scale_fill_gradient(limits=c(0,365),low="white",high="dark blue")

##save plot
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Grow_days_ann_0.5deg.png",height=2,width=8,dpi=1200)


#change resolution of climate data
SpatialGridDataFrame(bio_trop,proj4string=CRS("+proj=longlat +datum=WGS84"))

# Construct a SpatialPointsDataFrame from point data
head(Locations2)
Locations3<-data.frame(Locations2$Site,Locations2$Country,Locations2$Lat,Locations2$Long)
xy <- Locations3[4:3]
df<-Locations3[-4:-3]
Loc_points <- SpatialPointsDataFrame(coords=xy, data=df)
str(Locations3)
as(Loc_points,"SpatialPoints")

#extract data from grid
bio_loc<-overlay(y=as(Loc_points,"SpatialPoints"),x=as(bio_trop,"SpatialPolygonsDataFrame"))

#add climate data to locations
Locations3$gdays<-bio_loc$days
Locations3$temp<-bio_loc$temp

#rename columns

colnames(Locations3)<-c("Site","Country","Lat","Long","gdays","temp")
