#script to calculate total precipitation per year from bioclim data

#load libraries
library(sp)
library(rgdal)
library(ggplot2)

#load in spatial data - precipitation
Jan<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Climate/prec_10m_bil/prec1.bil")
Feb<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Climate/prec_10m_bil/prec2.bil")
March<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Climate/prec_10m_bil/prec3.bil")
April<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Climate/prec_10m_bil/prec4.bil")
May<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Climate/prec_10m_bil/prec5.bil")
June<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Climate/prec_10m_bil/prec6.bil")
July<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Climate/prec_10m_bil/prec7.bil")
August<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Climate/prec_10m_bil/prec8.bil")
Sep<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Climate/prec_10m_bil/prec9.bil")
Oct<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Climate/prec_10m_bil/prec10.bil")
Nov<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Climate/prec_10m_bil/prec11.bil")
Dec<-readGDAL("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Biomass accumulation in secondary forests/Analysis/Data/Climate/prec_10m_bil/prec12.bil")

#create a grid of total rainfall
Tot_rain<-Jan
Tot_rain<-(Jan@data+Feb@data+March@data+April@data+May@data+June@data+July@data+August@data+Sep@data+Oct@data+Nov@data+Dec@data)
Tot_rain_x<-rep((seq(-180,180,length.out=2160)),900)
Tot_rain_y<-(rep((seq(90.0,-60.0,length.out=900)),each=2160))
Total_rain<-data.frame(Tot_rain,Tot_rain_x,Tot_rain_y)
colnames(Total_rain)<-c("Total","x","y")

#subset to include only tropics
trop_total_rain<-subset(Total_rain,Total_rain$y>-30&Total_rain$y<30)
trop_total_rain$total2<-trop_total_rain$total
str(trop_total_rain)
trop_total_rain$total2[trop_total_rain$total2>2000]<-2000
(trop_total_rain)

#plot of growing season average temp growing season in year
a<-ggplot(trop_total_rain,aes(y=y,x=x,fill=Total))+borders("world", size=0.1,colour="grey",fill="lightgrey")+geom_raster(alpha=0.5)+theme_bw()+theme(panel.grid.major = element_line(colour =NA))+theme_bw()+theme(panel.grid.major = element_line(colour =NA))
b<-a+coord_cartesian(xlim = c(-130, 180),ylim=c(-30, 30))
b
b+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+scale_fill_gradient(high="dark blue",low="light grey",na.value =NA)

rm(list=ls())

##save plot
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Total_rainfall2.png",height=2,width=8,dpi=1200)
