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


head(AGB)

ggplot(AGB,aes(x=Sand,y=AGB))+geom_point()


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

#need to set up a rqandom effect to specify the allometric equations that
#was used in the original study or at the very least the broad type of allometric 
#equation that was used, or how biomass was calculated.


#
global_AGB1<-lmer(sqrt(AGB)~John+(1|ET_ID)+(1|Allo_type),data=AGB)
plot(fitted(global_AGB1),resid(global_AGB1))
summary(global_AGB1)
plot(global_AGB1)
qqnorm(global_AGB1)

AICc(global_AGB1)

global_AGB2<-lmer(sqrt(AGB)~M_Temp*T_Precip*Age+M_Temp*T_Precip*Age_log+M_Months*M_G_Temp*Age+M_Months*M_G_Temp*Age_log+Sand+(1|ET_ID)+(1|Allo_type),data=AGB)


plot(fitted(global_AGB2),resid(global_AGB2))
qqnorm(global_AGB1)
summary(global_AGB1)

r.squaredGLMM(global_AGB2)

plot(global_AGB2)
qqnorm(global_AGB2)
summary(global_AGB2)


AIC(global_AGB1,global_AGB2)

#and now do model averaging by running the dredge function
getAllTerms(global_AGB2)
Age<-AGB$Age

MS1<-dredge(global_AGB2,evaluate=T,rank=AICc,trace=T,REML=F,subset=!(Age&&Age_log)&!(M_Temp&&M_G_Temp)&!(M_Months&&T_Precip)&!(T_Precip&&M_G_Temp)&!(M_Months&&M_Temp))


#subset models with delta<7 (to remove implausible models)
poss_mod<- get.models(MS1, subset = delta<7)
modsumm <- model.sel(poss_mod, rank = "AICc",fit=T)
modsumm

Averaged<-model.avg(modsumm,fit=T)
Averaged

importance(Averaged)

head(AGB)

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



n <- length(poss_mod)

# Predictions from each of the models in a set, and with averaged coefficients
Age_pred<-predict(object=Averaged,newdata=newdata,Full=T,se.fit=T)

str(Age_pred)

#first age
Age<-seq(min(AGB$Age),max(AGB$Age),.5)
Pred_age<-24.71082870+(-5.25461632*(log(Age+1)))+Months_M+Temp_M+Age_Month+Age_Temp+Sand
Pred_age
plot(AGB$Age,AGB$AGB)
lines(exp(newdata$Age_log),(Age_pred$fit)^2)
lines(exp(newdata$Age_log),(Age_pred$fit+(Age_pred$se*1.96))^2,lty=2)
lines(exp(newdata$Age_log),(Age_pred$fit-(Age_pred$se*1.96))^2,lty=2)


#next months
newdata <- as.data.frame(lapply(lapply(AGB[c(10,12,14,17)], mean), rep, 300))
newdata$M_Months<-nseq(AGB$M_Months, nrow(newdata))
Months_pred<-predict(object=Averaged,newdata=newdata,Full=T,se.fit=T)

plot(AGB$M_Months,AGB$AGB)
lines(newdata$M_Months,Months_pred$fit^2)
lines(newdata$M_Months,(Months_pred$fit+(Months_pred$se*1.96))^2,lty=2)
lines(newdata$M_Months,(Months_pred$fit-(Months_pred$se*1.96))^2,lty=2)

#temp
newdata <- as.data.frame(lapply(lapply(AGB[c(10,12,14,17)], mean), rep, 300))
newdata$M_Temp<-nseq(AGB$M_Temp, nrow(newdata))
Temp_pred<-predict(object=Averaged,newdata=newdata,Full=T,se.fit=T)

plot(AGB$M_Temp,AGB$AGB)
lines(newdata$M_Temp,Temp_pred$fit^2)
lines(newdata$M_Temp,(Temp_pred$fit+(Temp_pred$se*1.96))^2,lty=2)
lines(newdata$M_Temp,(Temp_pred$fit-(Temp_pred$se*1.96))^2,lty=2)

#sand
#temp
newdata <- as.data.frame(lapply(lapply(AGB[c(10,12,14,17)], mean), rep, 300))
newdata$Sand<-nseq(AGB$Sand, nrow(newdata))
Sand_pred<-predict(object=Averaged,newdata=newdata,Full=T,se.fit=T)

plot(AGB$Sand,AGB$AGB,xlim=c(20,70),ylim=c(50,95))
lines(newdata$Sand,Sand_pred$fit^2)
lines(newdata$Sand,(Sand_pred$fit+(Sand_pred$se*1.96))^2,lty=2)
lines(newdata$Sand,(Sand_pred$fit-(Sand_pred$se*1.96))^2,lty=2)

#now interection between age and months
newdata1<- as.data.frame(lapply(lapply(AGB[c(10,12,14,17)], mean), rep, 300))
newdata1$Age_log<-nseq(AGB$Age_log, nrow(newdata))
newdata1$M_Months<-200
newdata2<- as.data.frame(lapply(lapply(AGB[c(10,12,14,17)], mean), rep, 300))
newdata2$Age_log<-nseq(AGB$Age_log, nrow(newdata))
newdata2$M_Months<-275
newdata3<- as.data.frame(lapply(lapply(AGB[c(10,12,14,17)], mean), rep, 300))
newdata3$Age_log<-nseq(AGB$Age_log, nrow(newdata))
newdata3$M_Months<-350

New_Age_M<-rbind(newdata1,newdata2,newdata3)

Age_month_pred<-predict(object=Averaged,newdata=New_Age_M,Full=T,se.fit=T)

New_Age_M$pred<-Age_month_pred$fit
New_Age_M$se<-Age_month_pred$se
head(New_Age_M)

Age_month<-ggplot(New_Age_M,aes(x=(exp(Age_log))-1,pred^2,group=as.factor(M_Months),colour=as.factor(M_Months)))+geom_line(size=2)
Age_month2<-Age_month+geom_line(data=New_Age_M,aes(y=(pred+(1.96*se))^2),lty=2,size=2)
Age_month2+geom_line(data=New_Age_M,aes(y=(pred-(1.96*se))^2),lty=2,size=2)

#now interaction between age and temp
plot(AGB$Age,AGB$M_Temp)

newdata1<- as.data.frame(lapply(lapply(AGB[c(10,12,14,17)], mean), rep, 300))
newdata1$Age_log<-nseq(AGB$Age_log, nrow(newdata))
newdata1$M_Temp<-24
newdata2<- as.data.frame(lapply(lapply(AGB[c(10,12,14,17)], mean), rep, 300))
newdata2$Age_log<-nseq(AGB$Age_log, nrow(newdata))
newdata2$M_Temp<-25
newdata3<- as.data.frame(lapply(lapply(AGB[c(10,12,14,17)], mean), rep, 300))
newdata3$Age_log<-nseq(AGB$Age_log, nrow(newdata))
newdata3$M_Temp<-26

New_Temp_M<-rbind(newdata1,newdata2,newdata3)

Age_Temp_pred<-predict(object=Averaged,newdata=New_Temp_M,Full=T,se.fit=T)

New_Temp_M$pred<-Age_Temp_pred$fit
New_Temp_M$se<-Age_Temp_pred$se
head(New_Age_M)

Age_Temp<-ggplot(New_Temp_M,aes(x=(exp(Age_log))-1,pred^2,group=as.factor(M_Temp),colour=as.factor(M_Temp)))+geom_line(size=2)
Age_Temp2<-Age_Temp+geom_line(data=New_Temp_M,aes(y=(pred+(1.96*se))^2),lty=2,size=2)
Age_Temp2+geom_line(data=New_Temp_M,aes(y=(pred-(1.96*se))^2),lty=2,size=2)



plot(AGB$Age,AGB$M_Month)

Temp_pred<-predict(object=Averaged,newdata=newdata,Full=T,se.fit=T)

plot(AGB$M_Temp,AGB$AGB)
lines(newdata$M_Temp,Temp_pred$fit^2)
lines(newdata$M_Temp,(Temp_pred$fit+(Temp_pred$se*1.96))^2,lty=2)
lines(newdata$M_Temp,(Temp_pred$fit-(Temp_pred$se*1.96))^2,lty=2)


AICc(global_AGB1)

global_AGB3<-lmer(sqrt(AGB)~Age_log*T_Precip+Age_log*M_Temp+(1|ET_ID)+(1|Allo_type),data=AGB)

summary(global_AGB3)

plot(AGB$Age,((log(AGB$Age+1))*-8.78500)^2)

plot(fitted(global_AGB3),resid(global_AGB3))
plot(AGB$Age,predict(global_AGB3)^2)
plot(AGB$T_Precip,predict(global_AGB3)^2)
plot(AGB$M_Temp,predict(global_AGB3)^2)

summary(global_AGB3)



summary(global_AGB3)


r.squaredGLMM(global_AGB3)

importance(Averaged)






predict(modsumm)



#output importance values
importance<-data.frame(Variable=c("Intercept",labels(importance(modsumm))),Importance=c(1,as.vector(importance(modsumm))))
importance

AGB$resid<-resid(global_AGB)
ggplot(data=AGB,aes(x=Long,y=Lat,colour=resid,size=abs(resid)))+geom_point(alpha=0.5)

ggplot(data=AGB,aes(x=abs(Lat),y=resid))+geom_point(alpha=0.5)
ggplot(data=AGB,aes(x=Long,y=resid))+geom_point(alpha=0.5)

predict(global_AGB)^2
