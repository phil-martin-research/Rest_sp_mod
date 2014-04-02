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
y<-unique(gsub("[[:punct:]]", "", gsub("[[:alpha:]]", "", x)))


for (i in 2:(length(x)-1)) {
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS")
  gs<-raster("GS.grd")
  z<-raster(x[i],sep="")
  Grow_season<-overlay(gs,z,fun=function(x,y){return(x+y)})
  writeRaster(Grow_season,filename="GS.grd",overwrite=T)
}

#save the result of this in folder as ascii file
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS")
z<-raster("GS.grd")
plot(z)
writeRaster(z,filename="GS",format="ascii")

#now we can calculate the growing season length in hours
x<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS",pattern='\\.grd$'))
y<-unique(gsub("[[:punct:]]", "", gsub("[[:alpha:]]", "", x)))
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS")
Months<-data.frame(days=c(31,28,31,30,31,30,31,31,30,31,30,31))
Months$cumdays<-cumsum((Months[,1]))
for (i in 2:12) {
  Months$mid[i]<-(round(Months$days[i]/2))
  Months$doy[i]<-Months$mid[i]+Months[i-1,2]
  Months$mid[1]<-0
  Months$doy[1]<-0
}
DOY<-data.frame(x=x[1:12],doy=Months$doy)

for (i in 1:12){
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS")
  z<-raster(x[i])
  z_points<-rasterToPoints(z)
  sub<-subset(z_points,z_points[,2]>=-60&z_points[,2]<=60)
  Theta<-0.2163108+(2*(atan(0.9671396*(tan(0.00860*(DOY[i,2]-186))))))
  Phi<-asin(0.39795*(cos(Theta)))
  Daylight<-(((sin((0.833*pi)/180))+(sin((sub[,2]*pi)/180))*(sin(Phi)))/((cos((sub[,2]*pi)/180))*(cos(Phi))))
  Daylight2<-24-((24/pi*(acos(Daylight))))
  D_GS<-rasterize(x=cbind(sub[,1],sub[,2]),y=z,field=Daylight2)
  GS_H<-overlay(z,D_GS,fun=function(x,y){return(x*y)})
  GS_H2<-GS_H*Months$days[i]
  plot(GS_H2)
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Hours")
  writeRaster(GS_H2,filename=paste("GS_Hours",y[i],".grd",sep=""),overwrite=T)
}


#now sum all of these hours into one map
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Hours")
x<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Hours",pattern='\\.grd$'))
y<-unique(gsub("[[:punct:]]", "", gsub("[[:alpha:]]", "", x)))
GSH<-raster(x[1])
writeRaster(GSH,filename="GSH.grd",overwrite=T)

for (i in 2:(length(x)-1)) {
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Hours")
  gsh<-raster("GSH.grd")
  z<-raster(x[i])
  Grow_season_hours<-overlay(gsh,z,fun=function(x,y){return(x+y)})
  plot(Grow_season_hours)
  writeRaster(Grow_season_hours,filename="GSH.grd",overwrite=T)
}

setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Hours")
gsh<-raster("GSH.grd")
hist(gsh)

#calculate mean temperature and precipiation during growing season for each location
x<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS",pattern='\\.grd$'))
t<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/tmean_10m_bil",pattern='\\.bil$'))
p<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/prec_10m_bil",pattern='\\.bil$'))
y<-unique(gsub("[[:punct:]]", "", gsub("[[:alpha:]]", "", x)))


#calculate the mean temperature and rainfall during the growing season
for (i in 1:(length(x)-1)) {
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS")
  gs<-raster(x[i])
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/tmean_10m_bil")
  temp<-raster(t[i])
  Grow_season_temp<-overlay(gs,temp,fun=function(x,y){return((x*y)/10)})
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Temp")
  writeRaster(Grow_season_temp,filename=paste("GS_temp",y[i],".grd",sep=""),overwrite=T)
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/prec_10m_bil")
  prec<-raster(p[i])
  Grow_season_prec<-overlay(gs,prec,fun=function(x,y){return((x*y))})
  plot(Grow_season_prec)
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Prec")
  writeRaster(Grow_season_prec,filename=paste("GS_prec",y[i],".grd",sep=""),overwrite=T)
}


#add all these growing season statistics together to
#produce the mean temperature during growing season
#and mean monthly rainfall during growing season
temp_x<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Temp",pattern='\\.grd$'))
prec_x<-sort(list.files("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Prec",pattern='\\.grd$'))
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Temp")
y<-unique(gsub("[[:punct:]]", "", gsub("[[:alpha:]]", "", x)))
temp_z<-raster(temp_x[3])
writeRaster(temp_z,"GS_temp.grd",overwrite=T)
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Prec")
prec_z<-raster(prec_x[3])
writeRaster(prec_z,"GS_prec.grd",overwrite=T)
for (i in 3:(length(temp_x)-1)) {
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Temp")
  gs_temp<-raster("GS_temp.grd")
  z<-raster(temp_x[i])
  Grow_season_temp<-overlay(gs_temp,z,fun=function(x,y){return(x+y)})
  writeRaster(Grow_season_temp,filename="GS_temp.grd",overwrite=T)
  setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Prec")
  gs_prec<-raster("GS_prec.grd")
  z_prec<-raster(prec_x[i])
  Grow_season_prec<-overlay(gs_prec,z_prec,fun=function(x,y){return(x+y)})
  writeRaster(Grow_season_prec,filename="GS_prec.grd",overwrite=T)
}

#save growing season temperature
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS")
GS<-raster("GS.grd")

str(GS)


setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Temp")
GS_temp<-raster("GS_temp.grd")
par(mfrow=c(1,1))
plot(GS_temp)
plot(GS)
GS_temp2<-overlay(GS_temp,GS,fun=function(x,y){return(x/y)})
plot(GS_temp2)
values(GS_temp2)<-ifelse(values(GS)==0,0,values(GS_temp2))
plot(GS_temp2)
summary(GS_temp2)
writeRaster(GS_temp2,filename="GS_temp_mean",format="ascii",overwrite=T)
writeRaster(GS_temp2,filename="GS_temp_mean.grd",overwrite=T)

#save growing season precipitation
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Prec")
GS_prec<-raster("GS_prec.grd")
GS_prec2<-GS_prec/GS
plot(sqrt(GS_prec2))
plot(log(GS_prec2))
writeRaster(GS_prec2,filename="GS_prec_mean",format="ascii")
writeRaster(GS_prec2,filename="GS_prec_mean.grd",overwrite=T)



map.GS<-rasterToPoints(GS)
map.T<-rasterToPoints(GS_temp2)
map.P<-rasterToPoints(GS_prec2)

df.GS<-data.frame(map.GS)
df.T<-data.frame(map.T)
df.P<-data.frame(map.P)

colnames(df.GS) <- c("Long","Lat", "Var")
colnames(df.T) <- c("Long","Lat", "Var")
colnames(df.P) <- c("Long","Lat", "Var")

Comb<-rbind(df.GS,df.T,df.P)



#plot on map

library(ggplot2)
theme_set(theme_bw(base_size=12))
a<-ggplot(df,aes(x=Long,y=Lat))+geom_raster(aes(fill=GS))
b<-a+opts(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
c<-b+scale_fill_gradient("Estimated growing\nseason (months)",low="light grey",high="dark green")+coord_equal()
c+ theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
setwd("C:/Users/Phil/Documents/My Dropbox/")
ggsave("GS.png",width=8,height=4,dpi=400,units="in")  




