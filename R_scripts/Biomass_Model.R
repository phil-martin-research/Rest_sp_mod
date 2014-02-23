###############################################################################
#script to produce model of biomass accumulation in tropical secondary forests#
###############################################################################

#Author: Phil Martin
#date of edit: 23/02/14

#load in packages
library(ggplot2)
library(plyr)
library(reshape2)
library(nlme)
library(lme4)
library(MuMIn)
library(gridExtra)

#open data files
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Data/Biomass")
AGB<-read.csv("Biomass_sites_climate.csv")
head(AGB)

#open climate grid
Grid<-read.csv("Climate_grid.csv")
Grid$M_Temp<-as.numeric(levels(Grid$M_Temp))[Grid$M_Temp]
Grid$M_Months<-Grid$M_Months*30
Grid$Deficit<-Grid$Deficit*10

AGB<-merge(AGB,Grid,by="ET_ID")
AGB<-AGB[complete.cases(AGB),]

#remove sites >40 years in age
#since I only want to predict to ~20 years
AGB<-subset(AGB,Age<=40)

#Calculate degree day years following Johnson et al
AGB$John<-(AGB$Age*AGB$M_Months*AGB$M_G_Temp)/365

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
plot(AGB$M_Months,log(AGB$AGB))
plot(AGB$M_G_Temp,sqrt(AGB$AGB))
plot(AGB$Age,AGB$AGB)
plot(AGB$John,AGB$AGB)
plot(AGB$Arid,log(AGB$AGB))
ggplot(AGB,aes(x=Age,y=Arid,size=AGB))+geom_point()
AGB_Sub<-AGB
pairs(AGB)

#null models to check for best random effect structure
null.1<-lme(sqrt(AGB)~1,random=~1|Study,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)
null.2<-lme(sqrt(AGB)~1,random=~1|Allo_type,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)
c(AICc(null.1),AICc(null.2))

#explore the best correlation structure
null.3<-lme(sqrt(AGB)~1,random=~1|Study,correlation = corGaus(1, form = ~ Lat2 + Long),data=AGB)
null.4<-lme(sqrt(AGB)~1,random=~1|Study,correlation = corLin(1, form = ~ Lat2 + Long),data=AGB)
null.5<-lme(sqrt(AGB)~1,random=~1|Study,correlation = corSpher(1, form = ~ Lat2 + Long),data=AGB)
c(AICc(null.3),AICc(null.4),AICc(null.5))


#models based on Johnson et al 2000 model
#one including a variabel describing sand content
#and one not including this
John_model<-lme(sqrt(AGB)~John+Sand,random=~1|Study,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)
Deg_years_model<-lme(sqrt(AGB)~John,random=~1|Study,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)

#our global model to include mean temp, mean precipitation
#number of growing days, mean temp during growing season
#sandiness of soils, aridity, water deficit
#and age
global_AGB2<-lme(sqrt(AGB)~M_Temp*T_Precip*Age+M_Months*M_G_Temp*Age+Sand+Arid*Age+Age*Deficit,random=~1|Study,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)

#Now do model averaging by running the dredge function
MS1<-dredge(global_AGB2,evaluate=T,rank=AICc,trace=T,REML=F,subset=!(M_Temp&&M_G_Temp)&!(M_Months&&T_Precip)&!(M_Months&&Arid)&!(M_Temp&&Arid)&!(T_Precip&&Arid)&!(M_G_Temp&&Arid)&!(M_G_Temp&&Deficit))

#subset models with delta<7 (to remove implausible models)
poss_mod<- get.models(MS1, subset = delta<7)
modsumm <- model.sel(poss_mod, rank = "AICc",fit=T)
modsumm<-subset(modsumm,delta<=7)
modsumm



#2 models come out on top
Mod1<-lme(sqrt(AGB+1)~M_Months+Age,random=~1|Study,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)
Mod2<-lme(sqrt(AGB)~Age+Sand,random=~1|Study,correlation = corExp(1, form = ~ Lat2 + Long),data=AGB)

#add rsquared to model ranking table
modsumm$R_squared<-c(r.squaredGLMM(Mod1)[1],r.squaredGLMM(Mod2)[1])

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

Averaged<-model.avg(modsumm,fit=T)

######################################################
#model of interaction between Age and growing period##
######################################################
head(AGB)
plot(AGB$Age,AGB$M_Months)
newdata1<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata1$Age <-nseq(AGB$Age,nrow(newdata1))
newdata1$M_Months<-200
newdata2<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
newdata2$Age<- nseq(AGB$Age, nrow(newdata2))
newdata2$M_Months<-250
newdata3<- as.data.frame(lapply(lapply(AGB[c(10,11,12,13,14,17)], mean), rep, 300))
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
Age_days2<-Age_days+geom_line(data=newdata,aes(y=pred^2,group=as.factor(M_Months),colour=as.factor(M_Months)),size=2)
Age_days3<-Age_days2+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Age_days4<-Age_days3+scale_colour_brewer("Estimated growing\nday categories",palette="Set1")
Age_days5<-Age_days4+ylab(expression(paste("Aboveground biomass (Mg ",ha^-1,")")))+xlab("Time since disturbance (Years)")
Age_days5

newdata<-rbind(newdata1,newdata2,newdata3)

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



Trop_pred<-predict(Averaged,Grid_rep,se.fit=T,level=0)

Grid_rep$pred<-Trop_pred$fit^2
Grid_rep$pred<-ifelse(Grid_rep$M_G_Temp==0,0,Grid_rep$pred)
Grid_rep$UCI<-((Trop_pred$fit)+(Trop_pred$se*1.96))^2
Grid_rep$UCI<-ifelse(Grid_rep$M_G_Temp==0,0,Grid_rep$pred)
Grid_rep$LCI<-((Trop_pred$fit)-(Trop_pred$se*1.96))^2
Grid_rep$UCI<-ifelse(Grid_rep$M_G_Temp==0,0,Grid_rep$pred)


#export grid
Grid_20<-subset(Grid_rep,Age==20)
Grid_30<-subset(Grid_rep,Age==30)
Grid_40<-subset(Grid_rep,Age==40)

setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results")
write.csv(Grid_20,"20_yr_pred.csv",row.names=F)
write.csv(Grid_30,"30_yr_pred.csv",row.names=F)
write.csv(Grid_40,"40_yr_pred.csv",row.names=F)

Grid_20<-read.csv("40_yr_pred.csv",)

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
Grid20.df3<-Grid20.df2[,c(1,2,6,17,18,19)]
Grid_melt<-melt(Grid20.df3,id.vars=c("group","long","lat"))
head(Grid_melt)





hist(Grid_melt$value)
summary(Grid_melt$value)

ggp <- ggplot(data=Grid_melt, aes(x=long, y=lat, group=group))
ggp2<-ggp+geom_polygon(data=Grid_melt,aes(fill=value))+facet_wrap(~variable,ncol=1)
ggp2+scale_fill_gradient(low="light grey",high="black",na.value=NA)+coord_equal()

setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures")
pdf("Biomass_param.pdf")
print(Age_days5)
print(Age_temp5)
print(Days_age5)
print(Temp_age5)
dev.off()
