###############################################################################
#script to produce model of biomass accumulation in tropical secondary forests#
###############################################################################

#Author: Phil Martin
#date of edit: 23/02/14

#clear environment
rm(list=ls())

#load in packages
library(ggplot2)
library(plyr)
library(reshape2)
library(nlme)
library(lme4)
library(MuMIn)
library(gridExtra)
library(ape)
library(geoR)

#open data files
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Biomass")
AGB<-read.csv("Biomass_sites_climate2.csv")
head(AGB)
AGB$GSH_Y<-AGB$Age*(AGB$GSH)

#remove sites >40 years in age
#since I only want to predict to ~20 years
#and AGB increases are non-linear 
#beyond this point
head(AGB)
AGB<-subset(AGB,Age<=40&Lat>-40&Lat<40)
AGB<-subset(AGB,Age<=40)
AGB<-AGB[complete.cases(AGB),]


#null models to check for best random effect structure
str(AGB)
null.1<-lme(sqrt(AGB)~1,random=~1|Study/Chr_ID,data=AGB)
null.2<-lme(sqrt(AGB)~1,random=~1|Chr_ID,data=AGB)

c(AICc(null.1),AICc(null.2))


#test the hypotheses that
#1.Biomass increases with cumulative growing season
#2.This relationship is dependant upon, precipitation and temp
#during this growing season
AGB<-subset(AGB,AGB>0)

BC_AGB<-boxcox((AGB)~GSH_Y, data = AGB,
       lambda = seq(-0.25, 1, length = 100))
Lamda<-BC_AGB$x[which.max(BC_AGB$y)]

AGB$AGB_L<-AGB$AGB^Lamda
null.1<-lme(AGB_L~1,random=~1|Study/Chr_ID,data=AGB)
Mod1<-lme(AGB_L~GSH_Y,random=~1|Study/Chr_ID,data=AGB)
Mod2<-lme(AGB_L~GSH_Y*GS_Temp,random=~1|Study/Chr_ID,data=AGB)
Mod3<-lme(AGB_L~GSH_Y*GS_Prec,random=~1|Study/Chr_ID,data=AGB)
Mod4<-lme(AGB_L~GSH_Y+GS_Temp,random=~1|Study/Chr_ID,data=AGB)
Mod5<-lme(AGB_L~GSH_Y+GS_Prec,random=~1|Study/Chr_ID,data=AGB)
Mod6<-lme(AGB_L~GSH_Y+Soil,random=~1|Study/Chr_ID,data=AGB)
Mod7<-lme(AGB_L~GSH_Y*GS_Temp+Soil,random=~1|Study/Chr_ID,data=AGB)
Mod8<-lme(AGB_L~GSH_Y*GS_Prec+Soil,random=~1|Study/Chr_ID,data=AGB)
Mod9<-lme(AGB_L~GSH_Y+GS_Temp+Soil,random=~1|Study/Chr_ID,data=AGB)
Mod10<-lme(AGB_L~GSH_Y+GS_Prec+Soil,random=~1|Study/Chr_ID,data=AGB)


plot(Mod1)
plot(Mod2)
plot(Mod3)
plot(Mod4)
plot(Mod5)
plot(Mod6)
plot(Mod7)
plot(Mod8)
plot(Mod9)
plot(Mod10)

#do some model selection of our hypothesis
mod.list<-list(null.1,Mod1,Mod2,Mod3,Mod4,Mod5,Mod6,Mod7,Mod8,Mod9,Mod10)

modsumm<-mod.sel(object=mod.list,rank="AICc",fit=T)
modsumm
modsumm<-subset(modsumm,delta<=7)
Averaged<-model.avg(modsumm,fit=T)

#create predictions based on growing season length

summary(AGB$GSH_Y)
theme_set(theme_bw(base_size=14))
AGB.preds<-data.frame(GSH_Y=seq(1160,176900,100),GS_Temp=mean(AGB$GS_Temp),GS_Prec=mean(AGB$GS_Prec))
GSH_Y_preds<-predict(Averaged,newdata=AGB.preds,level=0,se.fit=T)
AGB.preds$preds<-GSH_Y_preds$fit
AGB.preds$se<-GSH_Y_preds$se.fit
GY_plot1<-ggplot(AGB,aes(x=GSH_Y,y=AGB))+geom_point(shape=1,size=2)
GY_plot2<-GY_plot1+geom_line(data=AGB.preds,aes(y=preds^(1/Lamda)),size=1)
GY_plot3<-GY_plot2+geom_line(data=AGB.preds,aes(y=(preds+(1.96*se))^(1/Lamda)),size=1,lty=2)+geom_line(data=AGB.preds,aes(y=(preds-(1.96*se))^(1/Lamda)),size=1,lty=2)
GY_plot4<-GY_plot3+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
GY_plot5<-GY_plot4+xlab("Total accumulated growing season hours")+ylab("Above ground biomass")
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures")
ggsave(filename="GSY_AGB.pdf",width=8,height=6,units="in",dpi=400)

#create predictions for temperature

summary(AGB$GS_Temp)
AGB.preds_temp<-data.frame(GSH_Y=mean(AGB$GSH_Y),GS_Temp=seq(8.7,30.7,0.1),GS_Prec=mean(AGB$GS_Prec))
Temp_preds<-predict(Averaged,newdata=AGB.preds_temp,level=0,se.fit=T)
AGB.preds_temp$preds<-Temp_preds$fit
AGB.preds_temp$se<-Temp_preds$se.fit

Temp_plot1<-ggplot(AGB,aes(x=GS_Temp,y=AGB))+geom_point(shape=1,size=4)
Temp_plot2<-Temp_plot1+geom_line(data=AGB.preds_temp,aes(y=preds^(1/Lamda)),size=1)
Temp_plot2+geom_line(data=AGB.preds_temp,aes(y=(preds+(1.96*se))^(1/Lamda)),size=1,lty=2)+geom_line(data=AGB.preds_temp,aes(y=(preds-(1.96*se))^(1/Lamda)),size=1,lty=2)

Temp_plot1<-ggplot(data=AGB.preds_temp,aes(y=preds^(1/Lamda),x=GS_Temp))+geom_line()+geom_line(data=AGB.preds_temp,aes(y=(preds+(1.96*se))^(1/Lamda)),size=1,lty=2)+geom_line(data=AGB.preds_temp,aes(y=(preds-(1.96*se))^(1/Lamda)),size=1,lty=2)
Temp_plot2<-Temp_plot1+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Temp_plot3<-Temp_plot2+xlab("Average growing season temperature")+ylab("Above ground biomass")
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures")
ggsave(filename="GST_AGB.pdf",width=8,height=6,units="in",dpi=400)

plots<-list(GY_plot5,Temp_plot3,ggp5)

ml<-do.call(marrangeGrob, c(plots, list(nrow=1, ncol=1)))
ggsave("AGB_plots.pdf", ml,width=8,height=6,units="in",dpi=600)

#create predictions for precipitation
summary(AGB$GS_Prec)
AGB.preds_prec<-data.frame(GSH_Y=mean(AGB$GSH_Y),GS_Temp=mean(AGB$GS_Temp),GS_Prec=seq(45,392,1))
Preds_prec<-predict(Averaged,newdata=AGB.preds_prec,level=0,se.fit=T)
AGB.preds_prec$preds<-Preds_prec$fit
AGB.preds_prec$se<-Preds_prec$se.fit

GY_plot1<-ggplot(AGB,aes(x=GS_Prec,y=AGB))+geom_point(shape=1,size=4)
GY_plot1+geom_line(data=AGB.preds_prec,aes(y=preds^2),size=1)

ggplot(data=AGB.preds_prec,aes(y=preds^2,x=GS_Prec))+geom_line()+geom_line(data=AGB.preds_prec,aes(y=(preds+(1.96*se))^2),size=1,lty=2)+geom_line(data=AGB.preds_prec,aes(y=(preds-(1.96*se))^2),size=1,lty=2)


#################################################
##predictions over the entire tropics############
#################################################

#import growing season raster
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Hours")
GS<-raster("GSH.grd")
GS2<-((GS*20))
GS2<-aggregate(GS2,10)


par(mfrow=c(1,2))
#import precip raster
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Prec")
GS_prec<-raster("GS_prec_mean.grd")
GS_prec<-aggregate(GS_prec,10)
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Climate/GS/Temp")
GS_temp<-raster("GS_temp_mean.grd")
GS_temp<-aggregate(GS_temp,10)

#stack the three datasets
GS_stack<-stack(GS2,GS_prec,GS_temp)
#convert to df
GS.df<-data.frame(rasterToPoints(GS_stack))
colnames(GS.df)<-c("Long","Lat","GSH_Y","GS_Prec","GS_Temp")
summary(GS.df)
#get rid of NAs in dataframe
GS.df$GSH_Y[is.na(GS.df$GSH_Y)]<-0
GS.df$GS_Prec[is.na(GS.df$GS_Prec)]<-0
#set max as maximum of those in AGB
GS.df$GSH_Y<-ifelse(GS.df$GSH_Y>176909,176909,GS.df$GSH_Y)
GS.df$GS_Prec<-ifelse(GS.df$GS_Prec>393,393,GS.df$GS_Prec)
GS.df$GS_Temp<-ifelse(GS.df$GS_Temp>31,31,GS.df$GS_Temp)

#make predictions based model averaging
preds<-predict(Averaged,newdata=GS.df,level=0,se.fit=T)

GS.df$preds<-preds$fit^2
GS.df$uci<-(preds$fit+(1.96*preds$se.fit))^2
GS.df$lci<-(preds$fit-(1.96*preds$se.fit))^2
GS.df$preds<-ifelse(GS.df$GSH_Y==0,yes=0,no=GS.df$preds)

AGB_preds<-GS.df[,c(1,2,6,7,8)]
AGB_melt<-melt(AGB_preds,id.vars=c("Long","Lat))
str(AGB_melt)

#plot raw values
library(ggplot2)
theme_set(theme_bw(base_size=14))
ggp <- ggplot(data=AGB_melt, aes(x=Long, y=Lat))+geom_raster(aes(fill=value))+facet_wrap(~variable,ncol=1)
ggp2<-ggp+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
ggp3<-ggp2+scale_fill_gradient("Aboveground biomass\nafter 20 years of growth\n(Mg per ha)",low="light grey",high="dark green")+coord_equal()
ggp4<-ggp3+ylim(-40,40)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA),axis.title=element_text(size=14,face="bold"),title=element_text(size=14,face="bold"))
ggp5<-ggp4+ opts(axis.line=theme_blank(),axis.text.x=theme_blank(),
        axis.text.y=theme_blank(),axis.ticks=theme_blank(),
        axis.title.x=theme_blank(),
        axis.title.y=theme_blank())
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures")
ggsave("Biomass_map.pdf",width=8,height=8,dpi=300)
ggsave("Biomass_map.png",width=8,height=8,dpi=300)

#plot ranked values
library(ggplot2)
theme_set(theme_bw(base_size=14))
ggp <- ggplot(data=df.GS2, aes(x=Long, y=Lat))+geom_raster(aes(fill=as.factor(rank)))
ggp2<-ggp+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
ggp3<-ggp2+scale_fill_brewer(type="seq",aes(fill=as.factor(rank)))+coord_equal()
ggp4<-ggp3+ylim(-40,40)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA),axis.title=element_text(size=14,face="bold"),title=element_text(size=14,face="bold"))



print(Age_days5)
print(Age_temp5)
print(Days_age5)
print(Temp_age5)
dev.off()
