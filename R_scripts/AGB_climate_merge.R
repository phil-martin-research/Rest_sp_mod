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
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Analysis/Data/Climate")
grid_clim<- read.csv("Climate_soil_grid.csv")
head(grid_clim)
#import biomass site data
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Analysis/Data/Biomass")
Biomass<-read.csv("Biomass_sites_climate.csv")
head(Biomass)

#merge the two datasets
Biomass_clim<-merge(Biomass,grid_clim,by="ET_ID",all.x=T)
head(Biomass_clim)

pairs(Biomass_clim)

ggplot(Biomass_clim,aes(x=Age,y=AGB,group=ET_ID))+geom_point()+facet_wrap(~ET_ID)+geom_smooth(se=F,method="lm")