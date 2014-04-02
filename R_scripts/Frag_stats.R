library(raster)
library(SDMTools)
library(sp)
install.package("segue")

rm(list=ls())

setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Land cover/Tree_cover/2000")
Forest<-raster("Forest.tif")

e_cuts<-data.frame(from.x=c(-180,-180,-180,-180,0,0,0,0),
                   to.x=c(0,0,0,0,180,180,180,180),
                   from.y=c(-40,-20,0,20,-40,-20,0,20),
                   to.y=c(-20,0,20,40,-20,0,20,40))

rownames(e_cuts)<-NULL

for (i in 2:nrow(e_cuts)){
  e<-extent(c(e_cuts[i,1],e_cuts[i,2],e_cuts[i,3],e_cuts[i,4]))
  forest_e<-crop(Forest,e)
  Forest_2<- ConnCompLabel(forest_e)
  forest_patch<-PatchStat(Forest_2,cellsize=1120,latlon=T)
  forest_patch2<-forest_patch[-1,]
  m<-c(forest_patch2$patchID,forest_patch2$area/10000)
  rclmat <- matrix(m, ncol=2, byrow=F)
  forest_patch_size<-reclassify(Forest_2,rclmat)
  setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Land cover/Tree_cover/2000")
  writeRaster(forest_patch_size,filename=paste("For_patch_",i,".tif",sep=""))
}


test_grid<-replicate(10, round(runif(n=1000,0,1),0))
test_label<-ConnCompLabel(test_grid)
test_stats<-PatchStat(test_label)


setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Land cover/Tree_cover/2000")
writeRaster(forest_patch_size,filename="For_patch4.tif",)

