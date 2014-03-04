rm(list=ls())
library(raster)
library(ggplot2)


#this bit of code reads in data on temp and precipitation
#and sets growing season as all areas with temperature >0 degrees C
#with precipitation >10mm per month
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/tmean_10m_bil")
x<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/tmean_10m_bil",pattern='\\.tif$'))
p<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/prec_10m_bil",pattern='\\.bil$'))
y<-unique(gsub("[[:punct:]]", "", gsub("[[:alpha:]]", "", x)))

for (i in 1:length(x)) {
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/tmean_10m_bil")
  z<-raster(x[i])
  m<-c(-Inf,-1,0,0,180,1,180,500,0)
  m2<-matrix(m, ncol=3, byrow=TRUE)
  z2<-reclassify(z,m2)
  plot(z2)
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/prec_10m_bil")
  prec<-raster(p[i])
  rec_precip<-c(-Inf,10,0,10,Inf,1)
  rec_precip2<-matrix(rec_precip, ncol=3, byrow=TRUE)
  prec2<-reclassify(prec,rec_precip2)
  plot(prec2)
  GS<-overlay(prec2,z2,fun=function(x,y){return((x+y)/2)})
  rec_GS<-c(-Inf,.9,0,1,Inf,1)
  rec_GS<-matrix(rec_GS, ncol=3, byrow=TRUE)
  GS2<-reclassify(GS,rec_GS)
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/Frost")
  writeRaster(GS2,paste(y[i],"_frost.grd",sep=""),overwrite=T)
  plot(GS2)
}

#this code sets growing season for areas >18 degrees C
#and with rainfalls >100mm per month
for (i in 1:length(x)) {
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/tmean_10m_bil")
  z<-raster(x[i],sep="")
  m<-c(-Inf,180,0,180,Inf,1)
  m2<-matrix(m, ncol=3, byrow=TRUE)
  z2<-reclassify(z,m2)
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/prec_10m_bil")
  r<-raster(p[i],sep="")
  rc<-c(-Inf,100,0,100,Inf,1)
  rc2<-matrix(rc, ncol=3, byrow=TRUE)
  r2<-reclassify(r,rc2)
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/Tropical")
  Trop_grow<-overlay(z2,r2,fun=function(x,y){return(x*y)}, filename=paste(y[i],"_tropical.grd",sep=""),overwrite=T)
  plot(Trop_grow)
  
}

#this combines the two growing season statistics

x<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/Frost",pattern='\\.grd$'))
p<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/Tropical",pattern='\\.grd$'))
y<-unique(gsub("[[:punct:]]", "", gsub("[[:alpha:]]", "", x)))

for (i in 1:length(x)) {
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/Frost")
  z<-raster(x[i],sep="")
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/Tropical")
  r<-raster(p[i],sep="")
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS")
  Grow_season<-overlay(z,r,fun=function(x,y){return(x+y)},filename=paste(y[i],"_grow.grd",sep=""),overwrite=T)
}

#and this combines all months together

x<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS",pattern='\\.grd$'))
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS")
z<-raster(x[1],sep="")
writeRaster(z,"GS.grd",overwrite=T)
for (i in 2:(length(x)-1)) {
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS")
  gs<-raster("GS.grd")
  z<-raster(x[i],sep="")
  Grow_season<-overlay(gs,z,fun=function(x,y){return(x+y)})
  writeRaster(Grow_season,filename="GS.grd",overwrite=T)
}

setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS")
z<-raster("GS.grd")
plot(z)
writeRaster(z,filename="GS",format="ascii")
map.p<-rasterToPoints(z)
df<-data.frame(map.p)
head(df)
colnames(df) <- c("Long","Lat", "GS")

#plot on map
theme_set(theme_bw(base_size=12))
a<-ggplot(df,aes(x=Long,y=Lat))+geom_raster(aes(fill=GS))
b<-a+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
c<-b+scale_fill_gradient("Estimated growing\nseason (months)",low="light grey",high="dark green")+coord_equal()
c+ theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
setwd("C:/Users/Phil/Documents/My Dropbox/")
ggsave("GS.png",width=8,height=4,dpi=400,units="in")  


#alternative measure for growing season using difference between
#precipitation and potential evapotranspiration
# and temperatures >0 degrees C

rm(list=ls())

?gc
gc()
P_ET<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/PET_he_monthly",pattern='\\.tif$'))
P_ET
Prec<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/prec_10m_bil",pattern='\\.bil$'))
Temp<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/tmean_10m_bil",pattern='\\.bil$'))
y<-unique(gsub("[[:punct:]]", "", gsub("[[:alpha:]]", "", P_ET)))


memory.limit(000)


for (i in 1:length(x)) {
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/PET_he_monthly")
  P_ET1<-raster(P_ET[1])
  plot(P_ET1)
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/prec_10m_bil")
  Prec1<-raster(Prec[1])
  
  PET_Prec<-overlay(Prec1,P_ET1,fun=function(x,y){return(x-y)})
  plot(PET_Prec)
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/prec_10m_bil")
  prec<-raster(p[i])
  rec_precip<-c(-Inf,10,0,10,Inf,1)
  rec_precip2<-matrix(rec_precip, ncol=3, byrow=TRUE)
  prec2<-reclassify(prec,rec_precip2)
  plot(prec2)
  GS<-overlay(prec2,z2,fun=function(x,y){return((x+y)/2)})
  rec_GS<-c(-Inf,.9,0,1,Inf,1)
  rec_GS<-matrix(rec_GS, ncol=3, byrow=TRUE)
  GS2<-reclassify(GS,rec_GS)
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/Frost")
  writeRaster(GS2,paste(y[i],"_frost.grd",sep=""),overwrite=T)
  plot(GS2)
}

