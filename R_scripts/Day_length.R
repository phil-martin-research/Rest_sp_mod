# script to calculate day length
#import data
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Biomass")
AGB<-read.csv("Biomass_sites_climate_2.csv")
head(AGB)

Days<-expand.grid(Lat=seq(-60,60,1),Day=seq(1,365))

Days$theta<-0.2163108+(2*(atan(0.9671396*(tan(0.00860*(Days$Day-186))))))
Days$phi<-asin(0.39795*(cos(Days$theta)))
Days$Daylight<-(((sin((0.833*pi)/180))+(sin((Days$Lat*pi)/180))*(sin(Days$phi)))/((cos((Days$Lat*pi)/180))*(cos(Days$phi))))
Days$D<-24-((24/pi*(acos(Days$Daylight))))

Jan_theta<-0.2163108+(2*(atan(0.9671396*(tan(0.00860*(15-186))))))
Jan_phi<-asin(0.39795*(cos(Jan_theta)))
Jan_daylight<-(((sin((0.833*pi)/180))+(sin((AGB$Lat*pi)/180))*(sin(Jan_phi)))/((cos((AGB$Lat*pi)/180))*(cos(Jan_phi))))
Jan_D<-24-((24/pi*(acos(Jan_daylight))))
AGB$Jan2<-


AGB$Jan2<-



tapply(X=Days$D,Days$Lat,FUN=sum)


library(ggplot2)

ggplot(data=Days,aes(x=Day,y=D,group=as.factor(Lat),colour=as.factor(Lat)))+geom_line(size=2)
setwd("C:/Users/Phil/Documents/My Dropbox/")
ggsave(filename="Day_length.png")

