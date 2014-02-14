#script to produce figures of environmental data used in biomass
#accumulation work

#load libraries
library(sp)
library(rgdal)
library(ggplot2)
library(RODBC)
library(reshape)
library(raster)
library(rgeos)  
library(spdep)
library(gridExtra)

#get bioclim data on days in growing season and temp during growing season

bio_trop<-read.csv("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Climate data/Trop_bioclim2.csv",sep=",")
head(bio_trop)

#plot of number of days growing season in year
gsa<-ggplot(bio_trop,aes(y=y,x=x,fill=days))+borders("world", size=0.1,colour="grey",fill="lightgrey")+geom_raster(alpha=0.5)+theme_bw()+theme(panel.grid.major = element_line(colour =NA))+theme_bw()+theme(panel.grid.major = element_line(colour ="white"))
gsa
gsb<-gsa+coord_cartesian(xlim = c(-130, -20),ylim=c(-30, 30))
gsc<-gsb+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+scale_fill_gradient("Length of annual\ngrowing season (days)",limits=c(0,365),low="light blue",high="dark blue")
gsc
##save plot
setwd("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Figures")
ggsave(filename="Grow_days_ann_SA.jpeg",height=4,width=6,dpi=400)

#plot of growing season average temp growing season in year
tempa<-ggplot(bio_trop,aes(y=y,x=x,fill=temp))+borders("world", size=0.1,colour="grey",fill="lightgrey")+geom_raster(alpha=0.8)+theme_bw()+theme(panel.grid.major = element_line(colour =NA))+theme_bw()+theme(panel.grid.major = element_line(colour =NA))
tempb<-tempa+coord_cartesian(xlim = c(-130, -20),ylim=c(-30, 30))
tempc<-tempb+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+scale_fill_gradient("Mean temperature\nduring growing\nseason\n(degrees C)",limits=c(15,35),low="white",high="red",na.value =NA)
tempc
##save plot
setwd("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Figures")
ggsave(filename="Grow_temp_SA.jpeg",height=4,width=6,dpi=400)

#load in spatial data - soils

Soils_SA<-raster("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Soil data/HWSD_RASTER/SA.bil")
Soils_Agg<-aggregate(Soils_SA,fact=60,fun=median,filename="C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Soil data/HWSD_RASTER/SA_Agg.bil")
Soils_Agg2<-aggregate(Soils_SA,fact=12,filename="C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Soil data/HWSD_RASTER/SA_Agg2.bil")
plot(Soils_Agg)



#read in corse data
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
soilb<-soila+coord_cartesian(xlim = c(-130, -20),ylim=c(-30, 30))
soilc<-soilb+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+scale_fill_manual("soil type",values=c("NA","light grey","black"))
soilc
setwd("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Figures")
ggsave(filename="soil_type.jpeg",height=4,width=6,dpi=300)

