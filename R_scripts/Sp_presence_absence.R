#script for species pools

rm(list=ls())

library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(shapefiles)
library(plyr)
library(reshape2)
library(ggplot2)


#import grid to merge to
setwd("C:/Users/Phil/Desktop/All_species/")
grid_sp<- readShapePoly("2deg_land_grid")
Grid_ID<-grid_sp$ET_ID

#list csvs  that I want to import
x<-list.files("C:/Users/Phil/Desktop/All_species/Species_grid_csv/")
y<-unique(gsub("[[:punct:]]", "", gsub("[[:alpha:]]", "", x)))
Sp_code<-paste("600",y,sep="")

#loop to put a species code in each csv

for (i in 1:2) {
  setwd("C:/Users/Phil/Desktop/All_species/Species_grid_csv/")
  Sp<-read.csv(x[i])
  Sp$X<-NULL
  Sp$Sp_ID<-Sp_code[i]
  Sp$ET_ID<-Grid_ID[Sp$x]
  setwd("C:/Users/Phil/Desktop/All_species/Species_grid_csv_2/")
  write.csv(x=Sp,paste(y[i],".csv",sep=""),row.names=F)
}

for (i in 1:length(y)) {
  setwd("C:/Users/Phil/Desktop/All_species/Species_grid_csv_2")
  Sp<-read.csv(paste(y[i],".csv",sep=""))
  for (j in 1:nrow(Sp)) {
  Range<-Sp$ET_ID-Sp$ET_ID[j]
  Adj<-ifelse(Range<=181&Range>=179,yes=1,no=0)+
  ifelse(Range<=1&Range>=-1&Range!=0,yes=1,no=0)+
  ifelse(Range>=-181&Range<=-179,yes=1,no=0)
  Sp$Adj[j]<-ifelse(sum(Adj)>=1,1,0)
  }
  setwd("C:/Users/Phil/Desktop/All_species/Species_grid_csv3/")
  write.csv(x=Sp,paste(y[i],".csv",sep=""),row.names=F)
}

#put all species into same csv

#list csvs  that I want to import
x2<-list.files("C:/Users/Phil/Desktop/All_species/Species_grid_csv_2/")

Sp_grid<-do.call("rbind", lapply(x2, read.csv, header = TRUE))
head(Sp_grid)
Sp_grid$Sp_pool<-1

setwd("C:/Users/Phil/Desktop/All_species/Species_grid_csv_2/")
write.csv(x=Sp_grid,"Grid_Pres_Abs.csv")

