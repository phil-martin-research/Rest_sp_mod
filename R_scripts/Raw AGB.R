#script to import, analyse and produce plots for raw AGB in secondary forests

#load in necessary libraries
library(RODBC)
library(ggplot2)
library(nlme)
library(lme4)
library(MuMIn)


#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)

#import aboveground biomass query
AGB<- sqlFetch(sec, "Aboveground biomass query")
head(AGB)
#Rename columns
colnames(AGB) <- c("ID", "Site","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec","SS","Lat")
head(AGB)
levels(AGB$Type)
#Calculate aboveground biomass as a proportion of reference forest
AGB$Prop<-(AGB$AGB_Sec)/(AGB$AGB_Ref)
AGB$lnRR<-log(AGB$AGB_Sec)-log(AGB$AGB_Ref)
AGB$Proploss<-((AGB$AGB_Sec)-(AGB$AGB_Ref))/(AGB$AGB_Ref)
AGB$Proploss2<-(qlogis((AGB$Proploss+ 1) / 2))
plot(AGB$Age,((plogis(AGB$Proploss2)*2)-1)+1)

#subset data to remove logging, fire and missing values
AGB<-subset(AGB,AGB$Disturbance!="Fire")
AGB<-subset(AGB,AGB$Disturbance!="Logging")
AGB<-subset(AGB,AGB$Disturbance!="Agroforestry")
AGB<-subset(AGB,AGB$Type!="NA")
AGB<-subset(AGB,AGB$AGB_Sec!="0")
AGB<-subset(AGB,AGB$Age!="0")

#change types
levels(AGB$Type)[levels(AGB$Type)=="Tropical dry forest"] <- "Dry"
levels(AGB$Type)[levels(AGB$Type)=="Tropical moist forest"] <- "Moist"
levels(AGB$Type)[levels(AGB$Type)=="Tropical rainforest"] <- "Wet"
levels(AGB$Type)[levels(AGB$Type)=="Tropical montane forest"] <- "Montane"


#create column for reference as a factor

AGB$Ran<-as.factor(AGB$AGB_Ref)


ggplot(AGB,aes(y=Proploss2,x=Age))+geom_point()+facet_wrap(~Ran)

#redo sample sizes
AGB$SS[AGB$SS<0]<-1

#merge with climatic data (from other script)
AGB2<-merge(AGB,Locations3,by="Site")
AGB2<-subset(AGB2,AGB2$ID!="Becknell 2012")
AGB2<-subset(AGB2,AGB2$ID!="Letcher et al 2009")
AGB2<-subset(AGB2,AGB2$ID!="Saldarriaga et al 1988")

ggplot(AGB2,aes(y=AGB_Sec/Age,x=(Age*gdays*temp)/365))+geom_point()+geom_smooth(method="glm")
ggplot(AGB2,aes(y=AGB_Ref,x=(gdays*temp)/365))+geom_point()+geom_smooth(method="glm")

m1<-glm(log(AGB_Sec)~I((Age*gdays*temp)/365),data=AGB2)
m0<-lmer(log(AGB_Sec)~1+(1|Ran),data=AGB2)
1-(deviance(m1)/deviance(m0))
plot(m1)


#model checking looking at residuals
M1_res<-data.frame(predict(m1),predict(m1)-AGB2$AGB_Sec,AGB2$AGB_Sec)
colnames(M1_res)<-c("Pred","Resid","AGB")
plot(AGB2$Age,M1_res$Resid)
qplot(x=as.factor(AGB2$gdays),y=exp(predict(m1))-AGB2$AGB_Sec)+geom_boxplot()+geom_jitter()
ggplot(M1_res,aes(x=AGB,y=Resid))+geom_point()+geom_smooth(se=F)
qplot(x=AGB$Age,y=M1@resid)+geom_point()+geom_line(y=0,size=2)+geom_smooth(se=F)

summary(m1)

range(20*AGB2$gdays)
range(20*AGB2$temp)
range(AGB2$temp*AGB2$gdays)
range(20*AGB2$temp*AGB2$gdays)

m1sel<-dredge(m1,trace = TRUE, rank = "AICc", REML = FALSE)



poss_mod_m<- get.models(m1sel, subset = delta <9)
modsumm <- model.sel(poss_mod_m, rank = "AICc")
modsumm
modsumm<-subset(modsumm,modsumm$delta<9)
modsumm

new.data<-expand.grid(Age=seq(0.42,82,.5),gdays=seq(62,365,1),temp=seq(20,28.5,.1))
new.data$preds<-predict(m1,newdata=new.data)



