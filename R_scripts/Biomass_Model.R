#script to produce model of biomass accumulation

#load in packages
library(ggplot2)
library(plyr)
library(reshape2)
library(nlme)
library(MuMIn)

#open data files
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Analysis/Data/Biomass")
AGB<-read.csv("Biomass_sites_climate.csv")
head(AGB)


#open climate grid
Grid<-read.csv("Climate_grid.csv")
AGB<-merge(AGB,Grid,by="ET_ID")
AGB<-AGB[complete.cases(AGB),]
head(AGB)

#Calculate degree days
AGB$M_Months<-AGB$M_Months*30
AGB$John<-(AGB$Age*AGB$M_Months*AGB$M_G_Temp)/365

#calculate log an squared terms for age
AGB$Age_sq<-AGB$Age^2
AGB$Age_log<-log(AGB$Age+1)


head(AGB)

#exploratory analysis
hist(sqrt(AGB$AGB)) #gives more or less normal distibution of AGB
par(mfrow=c(1,1))
plot(AGB$M_Months,log(AGB$AGB))
plot(AGB$M_G_Temp,sqrt(AGB$AGB))
ggplot(data=AGB,aes(as.factor(Texture),AGB))+geom_boxplot()+geom_jitter()
plot(AGB$Age,AGB$AGB)
plot(AGB$John,AGB$AGB)
head(AGB)
ggplot(AGB,aes(Age,AGB))+geom_point()+facet_wrap(~ET_ID)
hist(AGB$AGB)



#set up model averaging
#first produce a global model


#
global_AGB1<-lme(sqrt(AGB)~John,random=~1|ET_ID,data=AGB)
global_AGB2<-lme(sqrt(AGB)~M_Temp*T_Precip*Age+M_Temp*T_Precip*Age_sq+M_Temp*T_Precip*Age_log+M_Months*M_G_Temp*Age+M_Months*M_G_Temp*Age_sq+M_Months*M_G_Temp*Age_log,random=~1|ET_ID,data=AGB)


plot(global_AGB1)
qqnorm(global_AGB1)
summary(global_AGB1)

r.squaredGLMM(global_AGB1)

plot(global_AGB2)
qqnorm(global_AGB2)
summary(global_AGB2)


AIC(global_AGB,global_AGB2)

#and now do model averaging by running the dredge function
getAllTerms(global_AGB2)
Age<-AGB$Age

MS1<-dredge(global_AGB2,evaluate=T,rank=AICc,trace=T,REML=F,m.max=5,subset=dc(Age,Age_sq)&!(Age&&Age_log)&!(Age_sq&&Age_log)&!(M_Temp&&M_G_Temp)&!(M_Months&&T_Precip))


#subset models with delta<7 (to remove implausible models)
poss_mod<- get.models(MS1, subset = delta<7)
modsumm <- model.sel(poss_mod, rank = "AICc")
modsumm<-subset(modsumm,modsumm$delta<7)

#output importance values
importance<-data.frame(Variable=c("Intercept",labels(importance(modsumm))),Importance=c(1,as.vector(importance(modsumm))))
importance

AGB$resid<-resid(global_AGB)
ggplot(data=AGB,aes(x=Long,y=Lat,colour=resid,size=abs(resid)))+geom_point(alpha=0.5)

ggplot(data=AGB,aes(x=abs(Lat),y=resid))+geom_point(alpha=0.5)
ggplot(data=AGB,aes(x=Long,y=resid))+geom_point(alpha=0.5)

predict(global_AGB)^2
