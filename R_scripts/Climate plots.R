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

#plot of number of days growing season in year
a<-ggplot(bio_trop,aes(y=y,x=x,fill=days))+borders("world", size=0.1,colour="grey",fill="lightgrey")+geom_raster(alpha=0.5)+theme_bw()+theme(panel.grid.major = element_line(colour =NA))+theme_bw()+theme(panel.grid.major = element_line(colour ="white"))
a
b<-a+coord_cartesian(xlim = c(-130, 180),ylim=c(-30, 30))
c<-b+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+scale_fill_gradient("Length of annual\ngrowing season (days)",limits=c(0,365),low="light blue",high="dark blue")
c
##save plot
setwd("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Figures")
ggsave(filename="Grow_days_ann.jpeg",height=2,width=8,dpi=400)

#plot of growing season average temp growing season in year
a<-ggplot(bio_trop,aes(y=y,x=x,fill=temp))+borders("world", size=0.1,colour="grey",fill="lightgrey")+geom_raster(alpha=0.8)+theme_bw()+theme(panel.grid.major = element_line(colour =NA))+theme_bw()+theme(panel.grid.major = element_line(colour =NA))
b<-a+coord_cartesian(xlim = c(-130, 180),ylim=c(-30, 30))
b+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+scale_fill_gradient("Mean temperature\nduring growing season\n(degrees C)",limits=c(15,35),low="white",high="red",na.value =NA)

##save plot
setwd("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Figures")
ggsave(filename="Grow_temp.png",height=2,width=8,dpi=400)
