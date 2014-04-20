#script to create rasters of each species projected
#probability of presence

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
x<-list.files("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Bird biodiversity data/Predictions")

for (i in 1:length(x)){
  setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Bird biodiversity data/Predictions")
  Sp<-read.csv(x[i])
  Sp_merge<-merge(grid_sp,Sp,by="ET_ID")
  Sp_merge@data$pred.y<-ifelse(is.na(Sp_merge@data$pred.y),0,Sp_merge@data$pred.y)
  r <- raster(extent(Sp_merge))
  res(r)<-2
  Sp_raster<-rasterize(Sp_merge, field="pred.y", r)
  setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Species")
  writeRaster(Sp_raster,paste("Species_",Sp[1,1],".tif",sep=""),overwrite=T)
}

#get EOO of each species based on grids

x2<-sub("Sp","",x)
x3<-sub(".csv","",x2)
EOO=matrix(data=NA, nrow=length(x), ncol=2)
for (i in 1:length(x3)){
  setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Bird biodiversity data/Predictions")
  Sp<-read.csv(x[i])
  EOO[i,1]<-as.numeric(x3[i])
  EOO[i,2]<-nrow(Sp)
}

EOO2<-data.frame(EOO)
colnames(EOO2)<-c("Species","EOO")
row.names(EOO2)<-NULL

EOO2$weight<-1/EOO2$EOO

EOO2$rank<-rank(EOO2$weight, ties.method = "first")

plot(EOO2$rank,EOO2$weight)

Zon<-data.frame(Weight=EOO2$weight,aplha=1,BQP1=1,BQP2=1,S=1,Sp_file=paste("Species_",EOO2$Species,".tif",sep=""))

setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Species")
write.csv(Zon,"Zon.csv",row.names=F)

setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Species")
Zon<-read.csv("Zon.csv")
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Species/Species")
Sp_r<-list.files("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Species/Species")
Sp_r[2029]
Sp_r[2051]
Sp_raster2<-raster(Sp_r[2029])
plot(Sp_raster2)
Sp_raster<-raster(Sp_r[1])
Sp_Prior<-Sp_raster*(Zon[1,1])
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Species/")
writeRaster(Sp_Prior,"Sp_Prior.tif",overwrite=T)

#produce maps of weighted values
for (i in 2226:length(Sp_r)){
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Species")
  Prior<-raster("Sp_Prior.tif")
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Species/Species")
  Sp<-raster(Sp_r[i])
  Sp2<-Sp*(Zon[i,1])
  Prior2<-overlay(Prior,Sp2,fun=function(x,y){return(x+y)})
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Species")
  writeRaster(Prior2,filename="Sp_Prior.tif",overwrite=T)
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Species/pdf")
  pdf(paste("Sp_Prior",i,".pdf",sep=""))
  par(mfrow=c(2,1))
  c(plot(Sp2),plot(Prior2))
  dev.off()
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Species/Stages")
  writeRaster(Prior2,filename=paste("Sp_Prior",i,".tif",sep=""),overwrite=T)
  print(i)
}


setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Species")
  
Sp_Prior<-raster("Sp_Prior.tif")
par(mfrow=c(1,1))
plot(Sp_Prior)

Sp_Prior2<-values(Sp_Prior)
hist(Sp_Prior2)
Rank_sp<-rank(Sp_Prior2,ties.method= "first")

plot(Sp_Prior2,Rank_sp)
summary(Rank_sp2)
Rank_sp2<-ifelse(is.na(Sp_Prior2),NA,Rank_sp)

?rank
plot(Sp_Prior2,Rank_sp2)

Sp_rank<-reclassify(Sp_Prior,cbind(Sp_Prior2,Rank_sp2/2563))

plot(Sp_rank)



