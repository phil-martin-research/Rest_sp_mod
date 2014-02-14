#script to produce climate space figures for paper

#load in packages
library(ggplot2)
library(plyr)
library(reshape2)

#open data files
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Analysis/Data/Climate")
Climate<-read.csv("All_sites_climate.csv")

#tidy up data
Climate$Temp<-Climate$Temp/10
Climate$Rain<-Climate$Precip
#subset out faulty data
Climate<-subset(Climate,Temp>0)

#produce convex hull
find_hull <- function(Climate) Climate[chull(Climate$Temp, Climate$Rain), ]
hulls <- ddply(Climate, "Study", find_hull)

#simple convext hull plot
theme_set(theme_bw(base_size=12))
ggplot(data=hulls,aes(x=Temp,y=Rain,colour=Study))+geom_polygon(fill=NA)+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA))+xlab("Mean annual temperature")+ylab("Mean annual rainfall")
