#script to analyse and reorganise data on soils at a global scale

#load libraries
library(sp)
library(rgdal)
library(ggplot2)
library(SDMTools)
library(reshape)
library(plyr)
library(RODBC)
library(raster)

#load in spatial data - soils

setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Analysis/Data/Soil data")

Soils_SA<-raster("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Analysis/Data/Soil data/hwsd.bil")
Soils_Agg<-aggregate(Soils_SA,fact=60,fun=median,filename="C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Analysis/Data/Soil data/SA_Agg.bil")
Soils_Agg2<-aggregate(Soils_SA,fact=12,filename="C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Soil data/HWSD_RASTER/SA_Agg2.bil")
plot(Soils_Agg)



#read in coarse data
Soils_Agg<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Soil data/HWSD_RASTER/SA_Agg.bil")
Soils_Tot<-round(Soils_Agg@data)
Soilx<-rep(seq(from=-130,to=-20,length.out=220),160)
Soily<-rep(seq(from=40,to=-40,length.out=160),each=220)
Total_soil<-data.frame(ID=Soils_Tot,Y=Soily,X=Soilx)

#read in corse data2
Soils_Agg2<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Soil data/HWSD_RASTER/SA_Agg2.bil")
Soils_Tot<-round(Soils_Agg2@data)



SoilX<-rep(seq(from=-130,to=-20,length.out=1100),800)
SoilY<-rep(seq(from=40,to=-40,length.out=800),each=1100)
Total_soil<-data.frame(ID=Soils_Tot,Y=SoilY,X=SoilX)

ggplot(data=Total_soil,aes(x=X,y=Y,fill=band1))+geom_raster(alpha=0.5)

#get data from WHSDB

HWSD<-read.csv("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Soil data/HWSD.csv",sep=",")
str(HWSD)
HWSD$ID<-HWSD$MU_GLOBAL
HWSD$MU_GLOBAL<-NULL
HWSD2<-data.frame(ID=HWSD$ID,Texture=HWSD$T_TEXTURE)

#merge HWSD with raster data
head(HWSD2)
M_soils<-merge(x=Total_soil,y=HWSD2,by.x="band1",by.y="ID",all.x=T)
M_soils$Texture2<-as.factor(M_soils$Texture)
levels(M_soils$Texture2)<-c("non-soil","sandy","not sandy","not sandy")

M_soils2<-M_soils[complete.cases(M_soils),]

#plot of soil type
theme_set(theme_bw(base_size=12))
soila<-ggplot(M_soils,aes(y=Y,x=X,fill=Texture2))+borders("world", size=0.1,colour="grey",fill="lightgrey")+geom_raster(alpha=0.9)+theme_bw()+theme(panel.grid.major = element_line(colour =NA))
soilb<-soila+coord_cartesian(xlim = c(-130, -20),ylim=c(-40, 40))
soilc<-soilb+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+scale_fill_manual("soil type",values=c("NA","light grey","black"))
soilc

##save plot
setwd("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Figures")
ggsave(filename="Soil_types.png",height=8,width=10,dpi=300)


#construct spatial datagrid from soils data
coordinates(M_soils2)<-c("X","Y")
gridded(M_soils2)<-T

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

# Construct a SpatialPointsDataFrame from point data
head(Locations2)
Locations3<-data.frame(Locations2$Site,Locations2$Country,Locations2$Lat,Locations2$Long)
xy <- Locations3[4:3]
df<-Locations3[-4:-3]
Loc_points <- SpatialPointsDataFrame(coords=xy, data=df)
str(Locations3)
as(Loc_points,"SpatialPoints")

#extract data from grid
?overlay
bio_loc<-overlay(y=as(Loc_points,"SpatialPoints"),x=as(Soils2,"SpatialPolygonsDataFrame"))
bio_loc
Locations3$bio_loc2<-bio_loc

?overlay
#Merge soil and site data
head(Locations3)
Locations4<-merge(x=Locations3,y=Soils3,by.x="bio_loc2",by.y="ID",all.x=T)
(Locations4)


