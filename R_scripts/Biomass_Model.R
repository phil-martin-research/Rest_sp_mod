#script to produce model of biomass accumulation

#load in packages
library(ggplot2)
library(plyr)
library(reshape2)
library(nlme)
library(lme4)
library(MuMIn)

sessionInfo()

#open data files
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Biomass")
AGB<-read.csv("Biomass_sites_climate.csv")
head(AGB)


#open climate grid

Grid<-read.csv("Climate_grid.csv")
str(Grid)
Grid$M_G_Temp<-as.numeric(levels(Grid$M_G_Temp))[Grid$M_G_Temp]
Grid$M_Temp<-as.numeric(levels(Grid$M_Temp))[Grid$M_Temp]
Grid$Sand<-as.numeric(levels(Grid$Sand))[Grid$Sand]

AGB<-merge(AGB,Grid,by="ET_ID")
AGB<-AGB[complete.cases(AGB),]
head(AGB)


#Calculate degree days
AGB$M_Months<-AGB$M_Months*30
AGB$John<-(AGB$Age*AGB$M_Months*AGB$M_G_Temp)/365

#calculate log an squared terms for age
AGB$Age_sq<-AGB$Age^2
AGB$Age_log<-log(AGB$Age+1)

#exploratory analysis
hist(sqrt(AGB$AGB)) #gives more or less normal distibution of AGB
plot(AGB$M_Months,log(AGB$AGB))
plot(AGB$M_G_Temp,sqrt(AGB$AGB))
plot(AGB$Age,AGB$AGB)
plot(AGB$John,AGB$AGB)

#set up model averaging
#first produce a global model
#this includes random effects to account for differences in how biomass was calculated
#and differences between grid squares to account for spatial autocorrelation
#Johnson et al 2000 model
global_AGB1<-lmer(sqrt(AGB)~John+(1|ET_ID)+(1|Allo_type),data=AGB)
plot(fitted(global_AGB1),resid(global_AGB1))

global_AGB2<-lmer(sqrt(AGB)~M_Temp*T_Precip*Age+M_Temp*T_Precip*Age_log+M_Months*M_G_Temp*Age+M_Months*M_G_Temp*Age_log+Sand+(1|ET_ID)+(1|Allo_type),data=AGB)
plot(fitted(global_AGB2),resid(global_AGB2))

#and now do model averaging by running the dredge function
getAllTerms(global_AGB2)
Age<-AGB$Age

MS1<-dredge(global_AGB2,evaluate=T,rank=AICc,trace=T,REML=F,subset=!(Age&&Age_log)&!(M_Temp&&M_G_Temp)&!(M_Months&&T_Precip)&!(T_Precip&&M_G_Temp)&!(M_Months&&M_Temp))


#subset models with delta<7 (to remove implausible models)
poss_mod<- get.models(MS1, subset = delta<7)
modsumm <- model.sel(poss_mod, rank = "AICc",fit=T)
modsumm<-subset(modsumm,delta<=7)
modsumm

Averaged<-model.avg(modsumm,fit=T)
Averaged

#output importance values
importance<-data.frame(Variable=c("Intercept",labels(importance(modsumm))),Importance=c(1,as.vector(importance(modsumm))))
importance


# New predictors: X1 along the range of original data, other
# variables held constant at their means
head(AGB)
newdata1<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata1$Age_log <- nseq(AGB$Age_log, nrow(newdata))
newdata1$M_Temp<-24
newdata2<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata2$Age_log <- nseq(AGB$Age_log, nrow(newdata))
newdata2$M_Temp<-25
newdata3<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata3$Age_log <- nseq(AGB$Age_log, nrow(newdata))
newdata3$M_Temp<-26

newdata<-rbind(newdata1,newdata2,newdata3)

Age_temp_pred<-predict(Averaged,newdata,se.fit=T)

newdata$pred<-Age_temp_pred$fit
newdata$se<-Age_temp_pred$se


#plot coefficients
a<-ggplot(data=AGB,aes(x=(exp(Age_log))-1,y=AGB))+geom_point()
a+geom_line(data=newdata,aes(y=pred^2,group=M_Temp,colour=M_Temp))

