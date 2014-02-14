#script to extract climate variables from bioclim data

library(sp)
library(rgdal)
library(ggplot2)

Pre_Jan<-readGDAL("C:/Users/Phil/Downloads/prec_10m_bil/prec1.bil")
str(Pre_Jan)
precip_Jan<-data.frame(Pre_Jan@data)

head(precip_Jan)
precip_Jan$x<-rep((seq(-180,180,length.out=2160)),900)
precip_Jan$y<-(rep((seq(90.0,-60.0,length.out=900)),each=2160))
sub_precip$band1<-(as.numeric(sub_precip$band1))
sub_precip<-subset(precip_Jan,precip_Jan$y>-30&precip_Jan$y<30)
sub_precip$days<-0
sub_precip$days[sub_precip$band1>100]<-31
sub_precip$band1[is.na(sub_precip$band1)]<-0

sub_precip2<-subset(sub_precip,sub_precip$x>-70&sub_precip$x<0)

#plot of number of rainfall for January
a<-ggplot(sub_precip,aes(y=y,x=x))+borders("world", size=0.1,colour="grey",fill="lightgrey")+geom_raster(alpha=0.5,aes(fill=band1))+theme_bw()+theme(panel.grid.major = element_line(colour =NA))+theme_bw()+theme(panel.grid.major = element_line(colour =NA))
b<-a+coord_cartesian(xlim = c(-130, 130),ylim=c(-30, 30))
b+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+opts(legend.position = "none")+scale_fill_gradient(low=NA,high="blue")

#save plot
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Precip_jan.png",height=3,width=6,dpi=1200)


#plot of number of days growing season for January
a<-ggplot(sub_precip,aes(y=y,x=x))+borders("world", size=0.1,colour="grey",fill="lightgrey")+geom_raster(alpha=0.5,aes(fill=days))+theme_bw()+theme(panel.grid.major = element_line(colour =NA))+theme_bw()+theme(panel.grid.major = element_line(colour =NA))
b<-a+geom_point(data=Locations2,aes(x=Long,y=Lat),colour="black")+ coord_cartesian(xlim = c(-130, 130),ylim=c(-30, 30))
b+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+opts(legend.position = "none")


setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Precip_jan.png",height=3,width=6,dpi=1200)
