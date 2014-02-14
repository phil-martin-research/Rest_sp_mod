rm(list=ls())

library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(shapefiles)
library(plyr)
library(reshape2)


#import grid to merge to
setwd("C:/Users/Phil/Desktop/All_species/")
grid_sp<- readShapePoly("2deg_land_grid")
head(grid_sp@data)

summary(grid_sp$ET_ID)

#list shapefiles that I want to import
x<-list.files("C:/Users/Phil/Desktop/All_species/Species_shp/")
y<-unique(gsub("[[:punct:]]", "", gsub("[[:alpha:]]", "", x)))


#####################
Test<-sample(y,size=90)
Test<-data.frame(Sp=Test,Fail=FALSE)


for (i in 1:1) {
  setwd("C:/Users/Phil/Desktop/All_species/Species_shp/")
  Sp1<-readShapePoly(paste("SPCRECID_", Test[i,1], ".shp", sep=""))
  Sp1$PRESENCE
  setwd("C:/Users/Phil/Desktop/All_species/Species_shape_pres")
  if (Sp1$PRESENCE<=2){
    Sp2<-Sp1[Sp1$PRESENCE<=2,]
    writePolyShape(Sp2, paste("Grid_pres_", Test[1,1],sep=""))
  }
  else{
    Test$Fail[1]<-TRUE
  } 
}


#loop to find size of shapefiles
file.check<-data.frame(Name=y,size=1)
for (i in 1:length(y)){
  setwd("C:/Users/Phil/Desktop/All_species/Species_shp/")
  size2<-file.info(paste("SPCRECID_", y[i], ".shp", sep=""))$size
  file.check$size[i]<-size2
}

#put large files in one subset and small files in another
Big_files<-subset(file.check,size>7000000)
Small_files<-subset(file.check,size<7000000)
head(Small_files)
Test<-sample(Small_files$Name,size=900)


#loop to produce csv files of the presence of each species
ptm <- proc.time()
for (i in 1:5){
  setwd("C:/Users/Phil/Desktop/All_species/Species_shp/")
  Sp1<-readShapePoly(paste("SPCRECID_", Small_files[1,1], ".shp", sep=""))
  Simp<-gSimplify(Sp1,tol=0.5,topologyPreserve=T)
  plot(Sp1)
  plot(Simp,add=T)
  possibleError <- tryCatch(
      Pres<-over(Sp1,grid_sp,returnList=T),
      Pres
      error=function(e) e
  )
  if(!inherits(possibleError, "error")){
  Pres2<-unique((melt(Pres))$value)
  Pres2
  Pres3<-data.frame(Present=c(Pres2),Type=1,Species=Small_files[1,1])
  Pres3
  colnames(Pres3)<-c("Presence","Type","Species")
  setwd("C:/Users/Phil/Desktop/All_species/Species_grid_csv/")
  write.csv(Pres2, paste("Grid_pres_", Small_files[i,1],".csv",sep=""))
 }
}
proc.time() - ptm

Pres3$ET_ID<-
  ET_ID<-grid_sp$ET_ID
ET_ID[Pres3$Presence]

#############################################

 setwd("C:/Users/Phil/Desktop/All_species/Species_shp/")
  Sp1<-readShapeSpatial(paste("SPCRECID_", y[2], ".shp", sep=""))
plot(Sp1)  
Sp1@data<-(Sp1@data[1])
  Sp1_1<-unionSpatialPolygons(Sp1,Sp1@data$SPCRECID)

plot(Sp1_1)
Pres<-over(Sp1,grid_sp,returnList=T)
  Pres2<-data.frame(Present=c(Pres[1]),Type=1)
  head(Pres2)
colnames(Pres2)<-c("Presence","Type")
Grid_pres<-merge(grid_sp,y=Pres2,by.x="ET_ID",by.y="Presence",all.x=F)
plot(Grid_pres)  
setwd("C:/Users/Phil/Desktop/All_species/Species_grids/")
  writePolyShape(Grid_pres, paste("Grid_pres", y[i]))

#import grid to merge to
setwd("C:/Users/Phil/Desktop/All_species/Grid_sp/")
grid_sp<- readShapePoly("Tropic_grid")
setwd("C:/Users/Phil/Desktop/All_species/Species_shp/")

Sp1<-readShapeSpatial("SPCRECID_1.shp")
Sp1@data<-(Sp1@data[1])
Sp1_1<-unionSpatialPolygons(Sp1,Sp1@data$SPCRECID)

Pres<-over(Sp1_1,grid_sp,returnList=T)
Pres
Pres2<-data.frame(Present=c(Pres[1]),Type=1)
head(Pres2)
Grid_pres<-merge(grid_sp,y=Pres2,by.x="ET_ID",by.y="X1",all.x=F)
setwd("C:/Users/Phil/Desktop/All_species/Species_grids/")
writePolyShape(Grid_pres, "Grid_pres_Sp1")



###############################

Sp1<-readShapeSpatial("SPCRECID_1007.shp")
Sp1
Sp1@data<-(Sp1@data[1])
Sp1_1<-unionSpatialPolygons(Sp1,Sp1@data$SPCRECID)

Pres<-over(Sp1_1,grid_sp,returnList=T)
Pres
Pres2<-data.frame(Present=Pres[1],Type=1)
colnames(Pres2)<-c("Presence","Type")
Grid_pres<-merge(grid_sp,y=Pres2,by.x="ET_ID",by.y="Presence",all.x=F)
plot(Grid_pres,col="red")
