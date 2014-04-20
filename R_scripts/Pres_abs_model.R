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
Cov<-read.csv("Bird_sites_cov.csv")
head(Abs_pres)
head(Cov)
PA<-merge(Abs_pres,Cov,by.x="Site_ID",by.y="SiteID")

#test models
#changed to included hierarchical site nested within grid ID
#this gives the probability of species being present at a site within
#the grid cell, rather than the probability of it being present at the broader scale

#remove rows with missing data
PA<-PA[complete.cases(PA),]
PA2<-subset(PA,EOO>10)
PA$Site_ID2<-as.factor(PA$Site_ID)
str(PA)


M0<-glmer(Pres~1+(1|Sp_ID)+(1|Grid)+(1|Site_ID2),data=PA,family=binomial)
M1<-glmer(Pres~Mean_0.05*F_dep+F_dep*EOO+(1|Sp_ID)+(1|Grid)+(1|Site_ID2),
          data=PA,family=binomial)
M2<-glmer(Pres~Mean_0.05*F_dep+EOO+(1|Sp_ID)+(1|Grid)+(1|Site_ID2),
          data=PA,family=binomial)
M3<-glmer(Pres~Mean_0.05+F_dep+EOO+(1|Sp_ID)+(1|Grid)+(1|Site_ID2),
          data=PA,family=binomial)
M4<-glmer(Pres~Mean_0.05*F_dep+(1|Sp_ID)+(1|Grid)+(1|Site_ID2),
          data=PA,family=binomial)
M5<-glmer(Pres~Mean_0.05+F_dep+(1|Sp_ID)+(1|Grid)+(1|Site_ID2),
          data=PA,family=binomial)
M6<-glmer(Pres~Mean_0.05+(1|Sp_ID)+(1|Grid)+(1|Site_ID2),
          data=PA,family=binomial)
M7<-glmer(Pres~F_dep+(1|Sp_ID)+(1|Grid)+(1|Site_ID2),
          data=PA,family=binomial)
M8<-glmer(Pres~EOO+(1|Sp_ID)+(1|Grid)+(1|Site_ID2),
          data=PA,family=binomial)
M9<-glmer(Pres~EOO*F_dep+(1|Sp_ID)+(1|Grid)+(1|Site_ID2),
          data=PA,family=binomial)
M10<-glmer(Pres~EOO*Mean_0.05+(1|Sp_ID)+(1|Grid)+(1|Site_ID2),
          data=PA,family=binomial)

summary(M1)


#model averaging of presence
mod.list<-list(M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10)
modsumm<-model.sel(object=mod.list,rank = "AICc",fit=T)
modsumm
Averaged<-model.avg(modsumm,subset=delta<7,fit=T)
Averaged
round(importance(Averaged),2)



#output model ranks
AICc_sel<-subset(modsumm,delta<=7)
AICc_sel$R_sq<-c(r.squaredGLMM(M2)[1],r.squaredGLMM(M3)[1])
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results")
write.csv(AICc_sel,"PA_mod.csv")

#output importance values with model averaged estimates
Imp_pres<-round(importance(Averaged),2)
write.csv(Imp_pres,"PA_Imp.csv")


#predict for cover and forest dependance
summary(PA)
df_preds<-data.frame(Mean_0.05=rep(seq(0,1,length.out=100),4),
           F_dep=as.factor(rep(c("High","Medium","Low","Non-forest"),each=100)),
           EOO=mean(PA$EOO))

preds<-predict(Averaged,newdata=df_preds,se.fit=T,backtransform=T)

df_preds$preds<-preds$fit
df_preds$uci<-df_preds$preds+(1.96*preds$se.fit)
df_preds$lci<-df_preds$preds-(1.96*preds$se.fit)

plot(df_preds$Mean_0.05,df_preds$preds,ylim=c(0,0.5))
points(df_preds$Mean_0.05,df_preds$uci)
points(df_preds$Mean_0.05,df_preds$lci)

df_preds$F_dep=factor(df_preds$F_dep, levels(df_preds$F_dep)[c(1,3,2,4)])



Abs_pres_data

ggplot(data=PA,aes(x=Mean_0.05,y=Pres))+facet_wrap(~F_dep)+geom_smooth(method="lm",formula=y ~ poly(x, 3))

theme_set(theme_bw(base_size=10))
a<-ggplot(data=df_preds,aes(x=Mean_0.05,y=preds,group=F_dep,colour=F_dep),size=1)+geom_line()+facet_wrap(~F_dep)
b<-a+geom_line(data=df_preds,aes(y=uci,group=F_dep,colour=F_dep),lty=2)
c<-b+geom_line(data=df_preds,aes(y=lci,fill=F_dep,group=F_dep,colour=F_dep,ymax=NULL,ymin=NULL),lty=2)
d<-c+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
e<-d+scale_colour_brewer("Forest dependancy",palette="Set1")+xlab(expression(paste("Percentage forest cover in 35 ",km^2," buffer",sep="")))
e+ylab("Probability of presence")+theme(axis.text.x  = element_text(size=10),axis.text.y  = element_text(size=10))+ theme(legend.position="none")
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures")
ggsave(filename="Cov_pres.pdf",width=8,height=4,units="in",dpi=400)

round(importance(Averaged),2)


#predict for EOO and forest dependance
summary(PA)
tapply(PA$EOO,PA$F_dep,min)
tapply(PA$EOO,PA$F_dep,max)

df_preds_EOO<-data.frame(EOO=c(seq(1,3.24e+07,length.out=100),seq(533,9.51e+07,length.out=100),
                           seq(110,1.10e+08,length.out=100),seq(0,1.36e+08,length.out=100)),
           F_dep=as.factor(rep(c("High","Medium","Low","Non-forest"),each=100)),
           Mean_0.05=mean(PA$Mean_0.05))

preds_EOO<-predict(Averaged,newdata=df_preds_EOO,se.fit=T,backtransform=T)

df_preds_EOO$preds<-preds_EOO$fit
df_preds_EOO$uci<-df_preds_EOO$preds+(1.96*preds_EOO$se.fit)
df_preds_EOO$lci<-df_preds_EOO$preds-(1.96*preds_EOO$se.fit)


df_preds_EOO$F_dep=factor(df_preds_EOO$F_dep, levels(df_preds_EOO$F_dep)[c(1,3,2,4)])

#make plots
require(scales)
theme_set(theme_bw(base_size=10))
a<-ggplot(data=df_preds_EOO,aes(x=EOO,y=preds,group=F_dep,colour=F_dep),size=1)+geom_line()+facet_wrap(~F_dep)
b<-a+geom_line(data=df_preds_EOO,aes(y=uci,group=F_dep,colour=F_dep),lty=2)
c<-b+geom_line(data=df_preds_EOO,aes(y=lci,fill=F_dep,group=F_dep,colour=F_dep,ymax=NULL,ymin=NULL),lty=2)
d<-c+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
e<-d+scale_colour_brewer("Forest dependancy",palette="Set1")+xlab(expression(paste("Species' global extent of occurance (",km^2,")",sep="")))
e+ylab("Probability of presence")+theme(axis.text.x  = element_text(size=10),axis.text.y  = element_text(size=10))+ theme(legend.position="none")+scale_x_continuous(labels=comma)+geom_rug(sides="b",alpha=0.5,position='jitter')
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures")
ggsave(filename="EOO_pres.pdf",width=8,height=4,units="in",dpi=400)


setwd("C:/Users/Phil/Desktop/All_species/Species_grid_csv_2")
Grid_extrap<-read.csv("Grid_Pres_Abs.csv")
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Bird biodiversity data/Site_data/Traits")
Terr<-read.csv("Traits_terrestrial_comp.csv")
For_cov<-read.csv("Forest_cov.csv")
Terr_grid<-merge(x=Grid_extrap,y=Terr,by.x="Sp_ID",by.y="Species_ID2")
Terr_grid2<-merge(x=Terr_grid,y=For_cov,by="ET_ID")
str(Terr_grid2)

head(as.numeric(as.character(Terr_grid2$MEAN)))

Terr_grid3<-data.frame(Sp_ID=Terr_grid2$Sp_ID,ET_ID=Terr_grid2$ET_ID,
                       F_dep=Terr_grid2$Forest_dep,EOO=Terr_grid2$EOO,
                       Mean_0.05=as.numeric(as.character(Terr_grid2$MEAN)))
Terr_grid3$Mean_0.05<-ifelse(is.na(Terr_grid3$Mean_0.05),0,Terr_grid3$Mean_0.05)
Sp1_pred<-predict(Averaged,newdata=Terr_grid3,backtransform=T,se.fit=T)
Sp1$pred<-Sp1_pred$fit
ggplot(Sp1,aes(x=EOO,y=pred,colour=F_dep))+geom_point()

Terr_sp<-unique(Terr_grid3$Sp_ID)
for (i in 2166:length(Terr_sp)){
  Sp1<-subset(Terr_grid3,Sp_ID==Terr_sp[i])
  Sp1_pred<-predict(Averaged,newdata=Sp1,backtransform=T,se.fit=T)
  Sp1$pred<-Sp1_pred$fit
  setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Bird biodiversity data/Predictions")
  write.csv(Sp1,paste("Sp",Terr_sp[i],".csv",sep=""),row.names=F)
}
