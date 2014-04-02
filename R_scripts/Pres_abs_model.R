#script for species pools

rm(list=ls())

library(sp)
library(raster)
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



#import files
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Bird biodiversity data/Site_data")
Abs_pres<-read.csv("Pres_abs.csv")
head(Abs_pres)

Abs_pres$CV_0.05<-Abs_pres$Std_0.05/Abs_pres$Cov_0.05

#test models
#changed to included hierarchical site nested within grid ID
#this gives the probability of species being present at a site within
#the grid cell, rather than the probability of it being present at the broader scale

#remove rows with missing data
Abs_pres<-Abs_pres[complete.cases(Abs_pres),]
Abs_pres$Site_ID2<-as.factor(Abs_pres$Site_ID)
str(Abs_pres)

M0<-glmer(Pres~1+(1|Sp_ID)+(1|Grid)+(1|Site_ID2),data=Abs_pres,family=binomial)

M1<-glmer(Pres~Cov_0.05*F_dep+CV_0.05*F_dep+EOO+Disp+(1|Sp_ID)+(1|Grid)+(1|Site_ID2),
          data=Abs_pres,family=binomial)


plot(M1)

summary(M1)

r.squaredGLMM(M1)

#model averaging of presence
Model_sel<-dredge(global.model=M1,rank=AICc,evaluate=T,trace=T)
Sel<-get.models(Model_sel,subset=delta<7)
modsumm<-model.sel(Sel, rank = "AICc",fit=T)
modsumm
Averaged<-model.avg(modsumm,subset=delta<7,fit=T)
Averaged
importance(Averaged)

#predict for cover and forest dependance
summary(Abs_pres)
df_preds<-data.frame(cov_norm=rep(seq(-40,28,length.out=100),4),
           F_dep=as.factor(rep(c("High","Medium","Low","Non-forest"),each=100)),
           EOO_norm=mean(Abs_pres$EOO_norm))

preds<-predict(Averaged,newdata=df_preds,se.fit=T,backtransform = T)


df_preds$preds<-preds$fit
df_preds$uci<-df_preds$preds+(1.96*preds$se.fit)
df_preds$lci<-df_preds$preds-(1.96*preds$se.fit)

df_preds$F_dep=factor(df_preds$F_dep, levels(df_preds$F_dep)[c(1,3,2,4)])

library(reshape2)
str(Abs_pres)
Abs_pres_cov<-Abs_pres[,c(5,10,17)]

str(Abs_pres_data)
Abs_pres_data<-ddply(Abs_pres_cov, .(cov_norm,F_dep), summarise,
   mean_pres=mean(Pres),min_SD=mean(Pres)-sd(Pres),
   max_SD =mean(Pres)+sd(Pres))

Abs_pres_data

ggplot(data=Abs_pres,aes(x=cov_norm,y=Pres))+geom_jitter()+facet_wrap(~F_dep)+geom_smooth()

theme_set(theme_bw(base_size=14))
a<-ggplot(data=df_preds,aes(x=cov_norm+mean(Abs_pres$cov),y=preds,group=F_dep,colour=F_dep,ymax=NULL,ymin=NULL),size=1)+geom_line()+facet_wrap(~F_dep)
b<-a+geom_line(data=df_preds,aes(y=uci,fill=F_dep,group=F_dep,colour=F_dep,ymax=NULL,ymin=NULL),lty=2)
c<-b+geom_line(data=df_preds,aes(y=lci,fill=F_dep,group=F_dep,colour=F_dep,ymax=NULL,ymin=NULL),lty=2)
d<-c+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
e<-d+scale_colour_brewer("Forest dependancy",palette="Set1")+xlab("Percentage tree cover in 0.1 degree cell")
e+ylab("Probability of presence")
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures")
ggsave(filename="Cov_pres.pdf",width=8,height=4,units="in",dpi=400)






plot(df_preds$cov_norm,df_preds$preds)

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

