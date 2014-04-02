#script to extract data on growing season length, 
#growing season temperature and growing season precipitation
#from raster calculations
rm(list=ls())
library(rgdal)
library(raster)
library(ggplot2)
library(maptools)

#import files
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS")
GS<-raster("GS.grd")
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Hours")
GSH<-raster("GSH.grd")
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Prec")
GS_prec<-raster("GS_prec_mean.grd")
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Temp")
GS_temp<-raster("GS_temp_mean.grd")
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Soil data")
Soil<-raster("Sand_2.tif")
plot(Soil)

setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Biomass")
AGB<-read.csv("Biomass_sites_climate_all.csv")
# Construct a SpatialPointsDataFrame
AGB$Age<-as.numeric(as.character(AGB$Age))
xy <- AGB[4:3]
sp<-SpatialPoints(xy)

AGB$GS<-extract(GS,sp,method="bilinear")
AGB$GSH<-extract(GSH,sp,method="bilinear")
AGB$GS_Temp<-extract(GS_temp,sp,method="bilinear")
AGB$GS_Prec<-extract(GS_prec,sp,method="bilinear")
AGB$Soil<-extract(Soil,sp,method="bilinear")


setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Biomass")

write.csv(AGB,"Biomass_sites_climate2.csv")


#do the same with forest and site data for bird bd data

setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Bird biodiversity data/Site_data")
Bird_sites<-read.csv("Bird_sites.csv")
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Bird biodiversity data/Site_data/Buffers")
Buffer_0.5<-readShapePoly("half_deg.shp")
Buffer_0.1<-readShapePoly("Tenth_deg.shp")
Buffer_0.05<-readShapePoly("Twentieth_deg.shp")
Buffer_0.01<-readShapePoly("100th_deg.shp")
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Land cover/Tree_cover/2000")
Cov<-raster("Cov_2000_2.tif")
Cov2<-raster("Cov_2000.tif")
F_Cov<-raster("Forst_cov.tif")
patches<-raster("For_patch.tif")

Mean_0.5<-extract(x=F_Cov,y=Buffer_0.5,fun = mean, weights = TRUE, small = TRUE)
Mean_0.1<-extract(x=F_Cov,y=Buffer_0.1,fun = mean, weights = TRUE, small = TRUE)
Mean_0.05<-extract(x=F_Cov,y=Buffer_0.05,fun = mean, weights = TRUE, small = TRUE)


Mean_p_0.5<-extract(x=patches,y=Buffer_0.5,fun = mean, weights = TRUE, small = TRUE,na.omit=T)
Mean_p_0.1 <- extract(x=patches, Buffer_0.1,fun=mean,weights = TRUE, small = TRUE,na.omit=T)
Mean_p_0.05<- extract(x=patches, Buffer_0.05,fun=mean,weights = TRUE, small = TRUE,na.omit=T)


Med_p_0.5<-extract(x=patches,y=Buffer_0.5,fun = median,na.omit=T)
Med_p_0.1 <- extract(x=patches, Buffer_0.1,fun=median,na.omit=T)
Med_p_0.05<- extract(x=patches, Buffer_0.05,fun=median,na.omit=T)

plot(Mean_0.05,Mean_p_0.05)
hist(Mean_p_0.05)

plot(Mean_0.5,Mean_0.05)

plot(Mean_0.05,Mean_0.05_fine)
abline(0,1)
Std_0.5<-extract(x=Cov2,y=Buffer_0.5,fun = sd)
Std_0.1 <- extract(Cov2 Buffer_0.1,fun=sd)
Std_0.05<- extract(Cov2, Buffer_0.05,fun=sd)

plot(Mean_0.05,log(Std_0.05/Mean_0.05))

plot(Mean_0.5,Mean_0.05)
points(Mean_0.5,Mean_0.1,col="red")
plot(Mean_0.5,Mean_0.05-Mean_0.1)


plot()

Bird_sites$Mean_0.5<-Mean_0.5
Bird_sites$Mean_0.1<-Mean_0.1
Bird_sites$Mean_0.05<-Mean_0.05

Bird_sites$Patch_0.5<-Mean_p_0.5
Bird_sites$Patch_0.1<-Mean_p_0.1
Bird_sites$Patch_0.05<-Mean_p_0.05

str(Bird_sites)
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Bird biodiversity data/Site_data")
write.csv(x=Bird_sites,file="Bird_sites_cov.csv")
