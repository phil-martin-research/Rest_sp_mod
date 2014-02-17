#script to produce model of biomass accumulation

#load in packages
library(ggplot2)
library(plyr)
library(reshape2)
library(nlme)
library(lme4)
library(MuMIn)
library(gridExtra)

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

#our global model to include mean temp, mean precipitation
#number of growing days, mean temp during growing season
#and age and log(age)
global_AGB2<-lmer(sqrt(AGB)~M_Temp*T_Precip*Age+M_Temp*T_Precip*Age_log+M_Months*M_G_Temp*Age+M_Months*M_G_Temp*Age_log+Sand+(1|ET_ID)+(1|Allo_type),data=AGB)
plot(fitted(global_AGB2),resid(global_AGB2))

#Now do model averaging by running the dredge function
MS1<-dredge(global_AGB2,evaluate=T,rank=AICc,trace=T,REML=F,subset=!(Age&&Age_log)&!(M_Temp&&M_G_Temp)&!(M_Months&&T_Precip))

#subset models with delta<7 (to remove implausible models)
poss_mod<- get.models(MS1, subset = delta<7)
modsumm <- model.sel(poss_mod, rank = "AICc",fit=T)
modsumm<-subset(modsumm,delta<=7)
Averaged<-model.avg(modsumm,fit=T)

#output importance values
importance<-data.frame(Variable=c("Intercept",labels(importance(modsumm))),Importance=c(1,as.vector(importance(modsumm))))
importance

#model of interaction between Age and growing period
head(AGB)
plot(AGB$Age,AGB$M_Months)
newdata1<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata1$Age_log <- nseq(AGB$Age_log, nrow(newdata1))
newdata1$M_Months<-200
newdata2<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata2$Age_log <- nseq(AGB$Age_log, nrow(newdata2))
newdata2$M_Months<-250
newdata3<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata3$Age_log <- nseq(AGB$Age_log, nrow(newdata3))
newdata3$M_Months<-300

newdata<-rbind(newdata1,newdata2,newdata3)

Age_temp_pred<-predict(Averaged,newdata,se.fit=T)

newdata$pred<-Age_temp_pred$fit
newdata$se<-Age_temp_pred$se


#plot coefficients
summary(AGB)
AGB$M_Months2<-ifelse(AGB$M_Months<225,200,AGB$M_Months2)
AGB$M_Months2<-ifelse(AGB$M_Months<275&AGB$M_Months>225,250,AGB$M_Months2)
AGB$M_Months2<-ifelse(AGB$M_Months>275,300,AGB$M_Months2)

#plots
theme_set(theme_bw(base_size=20))
Age_days<-ggplot(data=AGB,aes(x=(exp(Age_log))-1,y=AGB,colour=as.factor(M_Months2)))+geom_point(size=4,shape=1)
Age_days2<-Age_days+geom_line(data=newdata,aes(y=pred^2,group=as.factor(M_Months),colour=as.factor(M_Months)),size=2)
Age_days3<-Age_days2+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Age_days4<-Age_days3+scale_colour_brewer("Estimated growing\nday categories",palette="Set1")
Age_days5<-Age_days4+ylab(expression(paste("Aboveground biomass (Mg ",ha^-1,")")))+xlab("Time since disturbance (Years)")

#model of interaction between Age and Temp
head(AGB)
plot(AGB$Age,AGB$M_Months)
newdata1<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata1$Age_log <- nseq(AGB$Age_log, nrow(newdata1))
newdata1$M_Temp<-24
newdata2<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata2$Age_log <- nseq(AGB$Age_log, nrow(newdata2))
newdata2$M_Temp<-25
newdata3<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata3$Age_log <- nseq(AGB$Age_log, nrow(newdata3))
newdata3$M_Temp<-26

newdata<-rbind(newdata1,newdata2,newdata3)

Age_temp_pred<-predict(Averaged,newdata,se.fit=T)

newdata$pred<-Age_temp_pred$fit
newdata$se<-Age_temp_pred$se

#plot coefficients
summary(AGB)
AGB$M_Temp2<-ifelse(AGB$M_Temp<24.5,24,AGB$M_Temp2)
AGB$M_Temp2<-ifelse(AGB$M_Temp<25.5&AGB$M_Months>24.5,25,AGB$M_Temp2)
AGB$M_Temp2<-ifelse(AGB$M_Temp>25.5,26,AGB$M_Temp2)

plot(AGB$Age,AGB$M_Temp)


#plots
theme_set(theme_bw(base_size=20))
Age_temp<-ggplot(data=AGB,aes(x=(exp(Age_log))-1,y=AGB,colour=as.factor(M_Temp2)))+geom_point(size=4,shape=1)
Age_temp2<-Age_temp+geom_line(data=newdata,aes(y=pred^2,group=as.factor(M_Temp),colour=as.factor(M_Temp)),size=2)
Age_temp2
Age_temp3<-Age_temp2+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Age_temp4<-Age_temp3+scale_colour_brewer("Annual mean\ntemperature",palette="Set1")
Age_temp5<-Age_temp4+ylab(expression(paste("Aboveground biomass (Mg ",ha^-1,")")))+xlab("Time since disturbance (Years)")

#model of interaction between Temp and age
head(AGB)
plot(AGB$M_Temp,AGB$Age)
newdata1<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata1$Age_log <-log(0+1)
newdata1$M_Temp<-nseq(AGB$M_Temp, nrow(newdata2))
newdata2<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata2$Age_log <- log(20+1)
newdata2$M_Temp<-nseq(AGB$M_Temp, nrow(newdata2))
newdata3<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata3$Age_log <-log(40+1)
newdata3$M_Temp<-nseq(AGB$M_Temp, nrow(newdata2))

newdata<-rbind(newdata1,newdata2,newdata3)

Temp_age_pred<-predict(Averaged,newdata,se.fit=T)

newdata$pred<-Temp_age_pred$fit
newdata$se<-Temp_age_pred$se

#plot coefficients
summary(AGB)
AGB$Age2<-ifelse(AGB$Age<10,0,AGB$Age2)
AGB$Age2<-ifelse(AGB$Age<30&AGB$Age>10,20,AGB$Age2)
AGB$Age2<-ifelse(AGB$Age>30,40,AGB$Age2)

#plots
theme_set(theme_bw(base_size=20))
Temp_age<-ggplot(data=AGB,aes(x=M_Temp,y=AGB,colour=as.factor(Age2)))+geom_point(size=4,shape=1)
Temp_age
Temp_age2<-Temp_age+geom_line(data=newdata,aes(y=pred^2,group=as.factor((exp(Age_log)-1)),colour=as.factor(exp(Age_log)-1)),size=2)
Temp_age2
Temp_age3<-Temp_age2+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Temp_age4<-Temp_age3+scale_colour_brewer("Annual mean\ntemperature",palette="Set1")
Temp_age5<-Temp_age4+ylab(expression(paste("Aboveground biomass (Mg ",ha^-1,")")))+xlab("Mean annual temperature")

######################################################
#model of interaction between growing days and age####
######################################################

head(AGB)
plot(AGB$M_Months,AGB$Age)
newdata1<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata1$Age_log <-log(0+1)
newdata1$M_Months<-nseq(AGB$M_Months, nrow(newdata2))
newdata2<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata2$Age_log <- log(20+1)
newdata2$M_Months<-nseq(AGB$M_Months, nrow(newdata2))
newdata3<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata3$Age_log <-log(40+1)
newdata3$M_Months<-nseq(AGB$M_Months, nrow(newdata2))

newdata<-rbind(newdata1,newdata2,newdata3)

Days_age_pred<-predict(Averaged,newdata,se.fit=T)

newdata$pred<-Days_age_pred$fit
newdata$se<-Days_age_pred$se

#plot coefficients
summary(AGB)
AGB$Age2<-ifelse(AGB$Age<10,0,AGB$Age2)
AGB$Age2<-ifelse(AGB$Age<30&AGB$Age>10,20,AGB$Age2)
AGB$Age2<-ifelse(AGB$Age>30,40,AGB$Age2)

#plots
theme_set(theme_bw(base_size=20))
Days_age<-ggplot(data=AGB,aes(x=M_Months,y=AGB,colour=as.factor(Age2)))+geom_point(size=4,shape=1)
Days_age
Days_age2<-Days_age+geom_line(data=newdata,aes(y=pred^2,group=as.factor((exp(Age_log)-1)),colour=as.factor(exp(Age_log)-1)),size=2)
Days_age2
Days_age3<-Days_age2+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Days_age4<-Days_age3+scale_colour_brewer("Time since\nlast disturbance\n(years)",palette="Set1")
Days_age5<-Days_age4+ylab(expression(paste("Aboveground biomass (Mg ",ha^-1,")")))+xlab("Mean number of estimated growing days per year")

#plot all plots together

grid.arrange(Age_days5,Days_age5,Age_temp5,Temp_age5,ncol=2)

#################################################
##predictions over the entire tropics############
#################################################

#replicate data frame 3 times
Grid_rep<-do.call("rbind", replicate(3, Grid, simplify = FALSE))
Grid_rep$Age<-rep(seq(20,60,by=20),each=length(Grid$ET_ID))
Grid_rep$Age_log<-log(Grid_rep$Age+1)
Grid_rep<-Grid_rep[complete.cases(Grid_rep),]

Trop_pred<-predict(Averaged,Grid_rep,se.fit=T)

Grid_rep$pred<-Trop_pred$fit
Grid_rep$pred<-Trop_pred$fit


