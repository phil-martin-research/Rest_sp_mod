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
library(nlme)
library(lme4)
library(MuMIn)
library(twitteR)

setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Analysis/Data/Bird biodiversity data/Site_data")

Abs_pres<-read.csv("Pres_abs.csv")
head(Abs_pres)

Abs_pres2<-Abs_pres[sample(nrow(Abs_pres),500),]


#test models
#changed to included hierarchical site nested within grid ID
#this gives the probability of species being present at a site within
#the grid cell, rather than the probability of it being present at the broader scale



M1<-glmer(Pres~F_dep*Cover+Disp*Cover+Age+EOO+(1|Sp_ID)+(ID|Grid),data=Abs_pres,family=binomial)

M2<-glmer(Present~1+(1|Sp_ID)+Rep_ID|ET_ID.x),data=Abs_pres,family=binomial)

plot(Abs_pres$Cover)

summary(M1)

r.squaredGLMM(M1)

AICc(M1,M2)
summary(M1)


#model averaging of presence
Model_sel<-dredge(global.model=M1,rank=AICc,evaluate=T,trace=T)
Sel<-get.models(Model_sel,subset=delta<7)
modsumm<-model.sel(Sel, rank = "AICc")
modsumm
Averaged<-model.avg(modsumm,subset=delta<7)
Averaged
importance(Averaged)

#get dataset of all species in all grid cells
setwd("C:/Users/Phil/Desktop/All_species/Species_grid_csv_2/")
Sp_grid<-read.csv("Grid_Pres_Abs.csv")
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Analysis/Data/Bird biodiversity data/Site_data/Traits")
Sp_traits<-read.csv("Traits.csv")
head(Sp_traits)
Comb_traits<-merge(Sp_grid,Sp_traits,by.x="Sp_ID",by.y="Species_ID2",all.x=T)
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Analysis/Data/Bird biodiversity data/Site_data")
Range<-read.csv("BL_Range.csv")
Comb_traits2<-merge(Comb_traits,Range,by="Sp_ID")

setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Analysis/Data/Bird biodiversity data/Site_data/Traits")
Cover<-read.csv("Forest_cov.csv")
head(Comb_traits)
Comb_traits_cov<-merge(Comb_traits2,Cover,by.x="ET_ID",by.y="ET_ID",all.x=T)
head(Comb_traits_cov)

summary(Comb_traits_cov)

