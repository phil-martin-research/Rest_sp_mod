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


#open data files
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Biomass")
AGB<-read.csv("Biomass_sites_climate.csv")
head(AGB)
AGB$MT<-AGB$MT/10
AGB$G_Years<-AGB$Age*(AGB$GM/12)
AGB$G_Days<-AGB$Age*(AGB$GM/12)


plot(AGB$G_Years,AGB$AGB)

#open climate grid
Grid<-read.csv("Climate_grid.csv")
Grid$M_Temp<-as.numeric(levels(Grid$M_Temp))[Grid$M_Temp]
Grid$M_Months<-Grid$M_Months*30
AGB<-merge(AGB,Grid,by="ET_ID")
AGB<-AGB[complete.cases(AGB),]
AGB$Chr_ID<-as.factor(AGB$Chr_ID)
levels(AGB$Chr_ID)
AGB$GM<-AGB$GM*30
Grid$M_Months<-Grid$M_Months*30




#remove sites >40 years in age
#since I only want to predict to ~20 years
AGB<-subset(AGB,Age<=40)

plot(AGB$Age,AGB$AGB)
plot(AGB$G_Years,AGB$AGB)




ggplot(AGB,aes(Age,AGB))+geom_point()+facet_wrap(~Chr_ID)

#Calculate degree day years following Johnson et al
AGB$John<-(AGB$Age*AGB$GM*AGB$GST)/365

pairs(AGB[c(21,7,10,11,12,13,14,15,16,17,3,4,5)])

#adjust lat so that spatial autocorrelation can be accounted for
AGB$Lat2<-AGB$Lat+(rnorm(length(AGB$Lat),0,0.00001))
cs1Exp <- corExp(1, form = ~ Lat2 + Long)
cs1Exp <- Initialize(cs1Exp, AGB)
corMatrix(cs1Exp)[1:10, 1:4]

#distance matrix to test for autocorrelation
AGB.dists <- as.matrix(dist(cbind(AGB$Lon, AGB$Lat2)))
AGB.dists.inv <- 1/AGB.dists
diag(AGB.dists.inv) <- 0
AGB.dists.inv[1:5, 1:5]
Moran.I(AGB$AGB, AGB.dists.inv)#suprise, suprise there is autocorrelation

#exploratory analysis
hist(sqrt(AGB$AGB)) #gives more or less normal distibution of AGB
plot(AGB$GM,(AGB$AGB))
plot(AGB$GST,(AGB$AGB))
plot(AGB$Age,AGB$AGB)
plot(AGB$John,AGB$AGB)
plot(AGB$Def,AGB$AGB)
ggplot(AGB,aes(x=Age,y=Arid,size=AGB))+geom_point()


Studies<-sample(levels(AGB$Study),size=18)

AGB_sub<-subset(AGB,Study %in% Studies)
AGB_pred<-subset(AGB,!(Study %in% Studies))



#null models to check for best random effect structure
null.1<-lme(sqrt(AGB)~1,random=~1|Study/Chr_ID,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)
null.2<-lme(sqrt(AGB)~1,random=~1|Chr_ID,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)


anova(null.1,null.2)

Mod1<-lme(sqrt(AGB)~M_Months,random=~1|Allo_type/All/Study/Chr_ID/Age,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)
Mod2<-lme(sqrt(AGB)~Deficit+Sand,random=~1|Allo_type/All/Study/Chr_ID/Age,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)


r.squaredGLMM(Mod1)

plot(Mod1)
summary(Mod1)

new.data<-data.frame(M_Months=seq(0,12,0.5))
str(preds)

new.data$preds<-predict(Mod1,newdata=new.data,level=0)
head(preds)^2

plot(new.data$M_Months,new.data$preds^2)


c(AICc(null.1),AICc(null.2),AICc(null.3),AICc(null.4),AICc(null.5))
c(r.squaredGLMM(null.1),c(r.squaredGLMM(null.2)),c(r.squaredGLMM(null.3)),c(r.squaredGLMM(null.4)))
#explore the best correlation structure
null.4<-lme(sqrt(AGB)~1,random=~1|Allo_type/Study,correlation = corGaus(1, form = ~ Lat2 + Long),data=AGB)
null.5<-lme(sqrt(AGB)~1,random=~1|Allo_type/Study,correlation = corLin(1, form = ~ Lat2 + Long),data=AGB)
null.6<-lme(sqrt(AGB)~1,random=~1|Allo_type/Study,correlation = corSpher(1, form = ~ Lat2 + Long),data=AGB)

c(AICc(null.3),AICc(null.5),AICc(null.6))


#models based on Johnson et al 2000 model
#one including a variabel describing sand content
#and one not including this
John_model<-lme(sqrt(AGB)~John+Sand,random=~1|Allo_type/Study/Chr_ID,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)
Deg_years_model<-lme(sqrt(AGB)~John,random=~1|Allo_type/Study/Chr_ID,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)

summary(John_model)
summary(Deg_years_model)


Model1<-lme(sqrt(AGB)~Age+Deficit+Sand+0,random=~1|Allo_type/Study,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)
summary(Model1)
AICc(Model1)
plot(Model1)
plot(AGB$Sand,resid(Model1))
AICc(Model1)

#our global model to include mean temp, mean precipitation
#number of growing days, mean temp during growing season
#sandiness of soils, water deficit
#and age
global_AGB2<-lme(sqrt(AGB)~M_Temp*Age+T_Precip*Age+M_Months*Age+M_G_Temp*Age+Age*Deficit+Sand,random=~1|Study,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB_sub)

summary(global_AGB2)
plot(global_AGB2)
plot(AGB$Age,AGB$AGB)
qqnorm(global_AGB2)

#Now do model averaging by running the dredge function
MS1<-dredge(global_AGB2,evaluate=T,rank=AICc,trace=T,REML=F,subset=!(M_G_Temp&&!M_Months)&!(M_Temp&&M_G_Temp)&!(M_Months&&T_Precip)&!(M_Months&&M_Temp)&!(T_Precip&&Deficit)&!(M_G_Temp&&Deficit)&!(M_Months&&Deficit)&!(M_Temp&&Deficit))

#possible models

ggplot(AGB,aes(y=AGB,x=Allo_type))+geom_boxplot()
ggplot(AGB,aes(y=AGB,x=Age,group=as.factor(Chr_ID),colour=as.factor(Chr_ID)))+geom_smooth(method="lm",se=F)+ theme(axis.text.x=element_text(angle=90))+facet_wrap(~Allo_type)
ggplot(AGB,aes(y=AGB,x=Age,group=as.factor(Chr_ID),colour=as.factor(Chr_ID)))+geom_smooth(method="lm",se=F)+geom_point()theme(axis.text.x=element_text(angle=90))+facet_wrap(~Study)



Mod1<-lme(sqrt(AGB)~John,random=~1|Study/Chr_ID,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)

Mod2<-lme(sqrt(AGB)~Age,random=~1|Study/Chr_ID,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)
Mod3<-lme(sqrt(AGB)~G_Years,random=~1|Study/Chr_ID,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)
Mod4<-lme(sqrt(AGB)~GM*Age,random=~1|Allo_type/Study/Chr_ID,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)
Mod5<-lme(sqrt(AGB)~GM+Age*GST,random=~1|Allo_type/Study/Chr_ID,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)
Mod6<-lme(sqrt(AGB)~Age*Def,random=~1|Allo_type/Study/Chr_ID,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)



VarCorr(Mod3)

summary(Mod3)

str(ranef(Mod1))

plot(as.factor(rownames(ranef(Mod1)$Study)),ranef(Mod1)$Study[,1])

plot(AGB$G_Years,resid(Mod3))

plot(Mod3)


c(AICc(Mod1),AICc(Mod2),AICc(Mod3))

plot(AGB$John,predict(Mod1,level=0)^2)


mod.list<-list(Mod1,Mod2,Mod3,Mod5,Mod6)


Mod1

modsumm<-mod.sel(object=mod.list,rank="AICc",fit=T)
modsumm
modsumm<-subset(modsumm,delta<=7)
modsumm

plot(AGB$Def,log(AGB$AGB))


ggplot(AGB,aes(Age,GST,size=AGB))+geom_point()

Averaged<-model.avg(modsumm,fit=T)
importance(Averaged)
Averaged$avg

preds<-predict(Averaged,newdata=AGB,level=0)

plot(AGB$GST,preds^2)


library(plyr)
?count
count(AGB,"Chr_ID")

Alves<-subset(AGB,Chr_ID==2)
Alves_lm<-lm(AGB~Age+0,data=Alves)
plot(Alves_lm)
summary(Alves_lm)

Alves2<-subset(AGB,Chr_ID==3)
Alves_lm2<-lm(AGB~Age+0,data=Alves2)
plot(Alves_lm2)
summary(Alves_lm2)
plot(Alves2$Age,predict(Alves_lm2))

Stein<-subset(AGB,Chr_ID==4)
Stein_lm<-lm(AGB~Age+0,data=Stein)
plot(Stein$Age,Stein$AGB)
plot(Stein_lm)
summary(Stein_lm)
plot(Stein$Age,predict(Stein_lm))

Gehring<-subset(AGB,Chr_ID==42)
plot(Gehring$Age,Gehring$AGB)
Gehring_lm<-lm(log(AGB)~Age,data=Gehring)
plot(Gehring_lm)
plot(Gehring$Age,exp(predict(Gehring_lm)))

modsumm$R_squared<-c(r.squaredGLMM(Mod15)[1],r.squaredGLMM(Mod8)[1],r.squaredGLMM(Mod14)[1],r.squaredGLMM(Mod13)[1])

cor(Gehring$AGB)

ddply(AGB, "Chr_ID", function(df) coefficients(cor(AGB~Age, data=df)))

Slopes<-ddply(AGB, "Chr_ID", function(df) coefficients(lm(AGB~Age+0, data=df)))

Slopes2<-merge(Slopes,count(AGB,"Chr_ID"),by="Chr_ID")

Slopes3<-subset(Slopes2,freq>3)

Slopes4<-unique(merge(Slopes3,AGB,by="Chr_ID",all.y=F))

Slopes5<-subset(Slopes4,!duplicated(Slopes4$Chr_ID))

str(Slopes5)


plot(Slopes5$Sand,Slopes5$Age.x)

str(Slopes5)

pairs(Slopes5[,c(2,13,14,15,16,17,18,19,20)])

ggplot(Slopes5,aes(M_G_Temp,M_Months,size=Age.x))+geom_point()

null.mod<-lm(Age.x~1,data=Slopes5)
Mod1<-lm(Age.x~M_G_Temp,data=Slopes5)
Mod2<-lm(Age.x~M_Temp,data=Slopes5)
c(AICc(null.mod),AICc(Mod1),AICc(Mod2))

library(metafor)

data(lui)

head(lui)

metacor.DSL(lui$r.FDis, lui$n, lui$label,plot=T)

metacor.DSL(Slopes5$Age.x,Slopes5$freq,Slopes5$Study)
escalc(ri=Slopes5$Age.x,ni=Slopes5$freq,measure="COR")

plot(Mod2)
#4 models come out on top


#add johnson models to this table
John_sel<-dredge(John_model,evaluate=T,rank=AICc,trace=T,REML=F)
poss_John<-get.models(John_sel)
modsumm_John <- model.sel(poss_John, rank = "AICc",fit=T)
modsumm_John<-modsumm_John[1:3]
modsumm_John$R_squared<-c(r.squaredGLMM(Deg_years_model)[1],r.squaredGLMM(John_model)[1],r.squaredGLMM(null.1)[1])

#output model selection table
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results")
write.csv(modsumm,"Biomass_mod_sel.csv",row.names=F)
write.csv(modsumm_John,"Biomass_alt_models.csv",row.names=F)

#output importance values
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results")
importance<-data.frame(Variable=c("Intercept",labels(importance(modsumm))),Importance=c(1,as.vector(importance(modsumm))))
importance

write.csv(importance,"Biomass_importance.csv",row.names=F)


######################################################
#model of interaction between Age and growing period##
######################################################
nseq <- function(x, len = length(x)) seq(min(x, na.rm = TRUE),
    max(x, na.rm=TRUE), length = len)
head(AGB)
str(AGB)
plot(AGB$Age,AGB$M_Months)
newdata1<- as.data.frame(lapply(lapply(AGB[c(7,10,11,12,14)], mean), rep, 300))
newdata1$Age <-nseq(AGB$Age,nrow(newdata1))
newdata1$M_Months<-200
newdata2<- as.data.frame(lapply(lapply(AGB[c(7,10,11,12,14)], mean), rep, 300))
newdata2$Age<- nseq(AGB$Age, nrow(newdata2))
newdata2$M_Months<-250
newdata3<- as.data.frame(lapply(lapply(AGB[c(7,10,11,12,14)], mean), rep, 300))
newdata3$Age<- nseq(AGB$Age, nrow(newdata3))
newdata3$M_Months<-300

newdata<-rbind(newdata1,newdata2,newdata3)

Age_temp_pred<-predict(Averaged,newdata,se.fit=T,level=0)

newdata$pred<-Age_temp_pred$fit
newdata$se<-Age_temp_pred$se


#plot coefficients
summary(AGB)
AGB$M_Months2<-ifelse(AGB$M_Months<225,200,NA)
AGB$M_Months2<-ifelse(AGB$M_Months<275&AGB$M_Months>225,250,AGB$M_Months2)
AGB$M_Months2<-ifelse(AGB$M_Months>275,300,AGB$M_Months2)

#plots
theme_set(theme_bw(base_size=20))
Age_days<-ggplot(data=AGB,aes(x=Age,y=AGB,colour=as.factor(M_Months2)))+geom_point(size=4,shape=1)
Age_days2<-Age_days+geom_line(data=newdata,aes(y=pred^2,group=as.factor(M_Months),colour=as.factor(M_Months)),size=1)
Age_days3<-Age_days2+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Age_days4<-Age_days3+scale_colour_brewer("Estimated growing\nday categories",palette="Set1")
Age_days5<-Age_days4+ylab(expression(paste("Aboveground biomass (Mg ",ha^-1,")")))+xlab("Time since disturbance (Years)")
Age_days5


######################################################
#model of interaction between Age and growing period##
######################################################


str(AGB)
plot(AGB$Age,AGB$M_G_Temp)
newdata1<- as.data.frame(lapply(lapply(AGB[c(7,10,11,12,14)], mean), rep, 300))
newdata1$Age <-nseq(AGB$Age,nrow(newdata1))
newdata1$M_G_Temp<-15
newdata2<- as.data.frame(lapply(lapply(AGB[c(7,10,11,12,14)], mean), rep, 300))
newdata2$Age<- nseq(AGB$Age, nrow(newdata2))
newdata2$M_G_Temp<-20
newdata3<- as.data.frame(lapply(lapply(AGB[c(7,10,11,12,14)], mean), rep, 300))
newdata3$Age<- nseq(AGB$Age, nrow(newdata3))
newdata3$M_G_Temp<-25

newdata<-rbind(newdata1,newdata2,newdata3)

Age_temp_pred<-predict(Averaged,newdata,se.fit=T,level=0)

newdata$pred<-Age_temp_pred$fit
newdata$se<-Age_temp_pred$se


#plot coefficients
summary(AGB)
AGB$M_G_Temp2<-ifelse(AGB$M_G_Temp<17.5,15,NA)
AGB$M_G_Temp2<-ifelse(AGB$M_G_Temp<22.5&AGB$M_G_Temp>17.5,20,AGB$M_G_Temp2)
AGB$M_G_Temp2<-ifelse(AGB$M_G_Temp>22.5,25,AGB$M_G_Temp2)

#plot
theme_set(theme_bw(base_size=20))
Age_temp<-ggplot(data=AGB,aes(x=Age,y=AGB,colour=as.factor(M_G_Temp2)))+geom_point(size=4,shape=1)
Age_temp2<-Age_temp+geom_line(data=newdata,aes(y=pred^2,group=as.factor(M_G_Temp),colour=as.factor(M_G_Temp)),size=1)
Age_temp3<-Age_temp2+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Age_temp4<-Age_temp3+scale_colour_brewer("Mean temperature\nduring growing season",palette="Set1")
Age_temp5<-Age_temp4+ylab(expression(paste("Aboveground biomass (Mg ",ha^-1,")")))+xlab("Time since disturbance (Years)")
Age_temp5

Age_temp_pred<-predict(Averaged,newdata,se.fit=T)

newdata$pred<-Age_temp_pred$fit
newdata$se<-Age_temp_pred$se

#plot coefficients
summary(AGB$M_G_Temp)
summary(AGB$M_G_Temp2)
AGB$M_G_Temp2<-ifelse(AGB$M_G_Temp<17.5,15,NA)
AGB$M_G_Temp2<-ifelse(AGB$M_G_Temp<22.5&AGB$M_G_Temp>17.5,20,AGB$M_G_Temp2)
AGB$M_G_Temp2<-ifelse(AGB$M_G_Temp>22.5,25,AGB$M_G_Temp2)

plot(AGB$Age,AGB$M_G_Temp)


#plots
theme_set(theme_bw(base_size=20))
Age_temp<-ggplot(data=AGB,aes(x=(exp(Age_log))-1,y=AGB,colour=as.factor(M_G_Temp2)))+geom_point(size=4,shape=1)
Age_temp2<-Age_temp+geom_line(data=newdata,aes(y=pred^2,group=as.factor(M_G_Temp),colour=as.factor(M_G_Temp)),size=2)
Age_temp2
Age_temp3<-Age_temp2+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Age_temp4<-Age_temp3+scale_colour_brewer("Mean temperature\nduring growing season",palette="Set1")
Age_temp5<-Age_temp4+ylab(expression(paste("Aboveground biomass (Mg ",ha^-1,")")))+xlab("Time since disturbance (Years)")

######################################################
#model of interaction between growing days and age####
######################################################

head(AGB)
plot(AGB$M_Months,AGB$Age)
newdata1<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata1$Age <-5
newdata1$M_Months<-nseq(AGB$M_Months, nrow(newdata2))
newdata2<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata2$Age<- 20
newdata2$M_Months<-nseq(AGB$M_Months, nrow(newdata2))
newdata3<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata3$Age<-40
newdata3$M_Months<-nseq(AGB$M_Months, nrow(newdata2))

newdata<-rbind(newdata1,newdata2,newdata3)

Days_age_pred<-predict(Averaged,newdata,se.fit=T,level=0)

newdata$pred<-Days_age_pred$fit
newdata$se<-Days_age_pred$se

#plot coefficients
AGB$Age2<-ifelse(AGB$Age<12.5,5,NA)
AGB$Age2<-ifelse(AGB$Age<30&AGB$Age>12.5,20,AGB$Age2)
AGB$Age2<-ifelse(AGB$Age>30,40,AGB$Age2)

#plots
theme_set(theme_bw(base_size=20))
Days_age<-ggplot(data=AGB,aes(x=M_Months,y=AGB,colour=as.factor(Age2)))+geom_point(size=4,shape=1)
Days_age
Days_age2<-Days_age+geom_line(data=newdata,aes(y=pred^2,group=as.factor(Age),colour=as.factor(Age)),size=2)
Days_age2
Days_age3<-Days_age2+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Days_age4<-Days_age3+scale_colour_brewer("Time since\nlast disturbance\n(years)",palette="Set1")
Days_age5<-Days_age4+ylab(expression(paste("Aboveground biomass (Mg ",ha^-1,")")))+xlab("Mean number of \nestimated growing\n days per year")

#plot all plots together

setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures")
pdf(file = "Biomass_grid.pdf",width=20,height=10)
grid.arrange(Age_days5,Days_age5,Age_temp5,Temp_age5,ncol=2)
dev.off()

#################################################
##predictions over the entire tropics############
#################################################

#replicate data frame 3 times
Grid_rep<-do.call("rbind", replicate(3, Grid, simplify = FALSE))
Grid_rep$Age<-rep(seq(20,40,by=10),each=length(Grid$ET_ID))
Grid_rep<-Grid_rep[complete.cases(Grid_rep),]
Grid_rep$John<-(Grid_rep$Age*Grid_rep$M_Months*Grid_rep$M_G_Temp)/365

hist(Grid_rep$John)
summary(Grid_rep$John)

Trop_pred<-predict(Mod1,Grid_rep,se.fit=T,level=0)
summary(Trop_pred$fit^2)
Grid_rep$pred<-Trop_pred$fit^2
Grid_rep$pred<-ifelse(Grid_rep$Biome==0,0,Grid_rep$pred)
Grid_rep$UCI<-((Trop_pred$fit)+(Trop_pred$se*1.96))^2
Grid_rep$UCI<-ifelse(Grid_rep$Biome==0,0,Grid_rep$pred)
Grid_rep$LCI<-((Trop_pred$fit)-(Trop_pred$se*1.96))^2
Grid_rep$LCI<-ifelse(Grid_rep$Biome==0,0,Grid_rep$pred)

summary(Grid_rep)





#export grid
Grid_20<-subset(Grid_rep,Age==20)
Grid_30<-subset(Grid_rep,Age==30)
Grid_40<-subset(Grid_rep,Age==40)

setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results")
write.csv(Grid_20,"20_yr_pred.csv",row.names=F)
write.csv(Grid_30,"30_yr_pred.csv",row.names=F)
write.csv(Grid_40,"40_yr_pred.csv",row.names=F)

Grid_20<-read.csv("20_yr_pred.csv",)

#load in shapefile
library(rgdal)
library(foreign)
setwd("C:/Users/Phil/Desktop/All_species")
Land_grid<-read.dbf("2deg_land_grid.dbf")
Land_grid<-subset(Land_grid,select=-c(2:22))
Land_grid2<-data.frame(ET_ID=unique(Land_grid$ET_ID))
Land_grid20<- merge(Land_grid2, Grid_20, by="ET_ID", all=TRUE)
head(Land_grid20)
write.dbf(Land_grid20, "2deg_land_grid.dbf")



#load in data
detach("package:lme4", unload=TRUE)
Grid20<-readOGR("2deg_land_grid.shp","2deg_land_grid")
Grid20@data$id<-rownames(Grid20@data)
Grid20.df<-fortify(Grid20)
head(Grid20.df)
Grid20.df2<-join(Grid20.df, Grid20@data, by="id")
head(Grid20.df2)
str(Grid20.df2)
Grid20.df3<-Grid20.df2[,c(1,2,6,19,20,21)]
Grid_melt<-melt(Grid20.df3,id.vars=c("group","long","lat"))
head(Grid_melt)





hist(Grid_melt$value)
summary(Grid_melt$value)

ggp <- ggplot(data=Grid_melt, aes(x=long, y=lat, group=group))
ggp2<-ggp+geom_polygon(data=Grid_melt,aes(fill=value))+facet_wrap(~variable,ncol=1)
ggp2+scale_fill_gradient(low="light grey",high="dark green",na.value=NA)+coord_equal()

setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures")
pdf("Biomass_param.pdf")
print(Age_days5)
print(Age_temp5)
print(Days_age5)
print(Temp_age5)
dev.off()
