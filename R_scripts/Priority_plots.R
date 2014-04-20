# plots of biomass and species priorities
library(ggplot2)
library(plyr)
library(reshape2)
library(raster)
library(gridExtra)

#first species
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Species")
Prior<-raster("BD_for.tif")
Base<-raster("Sp_Prior.tif")  



plot(Prior)

#convert the raster to points for plotting
map.prior <- rasterToPoints(Prior)
map.base <- rasterToPoints(Base)

#Make the points a dataframe for ggplot
df.prior <- data.frame(map.prior)
df.base <- data.frame(map.base)
#Make appropriate column headings
colnames(df.prior) <- c("Long", "Lat", "Prior")
colnames(df.base) <- c("Long", "Lat", "Prior")
df.prior$Rank<-rank(df.prior$Prior)
df.prior$Type<-"Bird biodiversity"

df.base.2<-(df.base)
df.base$Type<-"Bird biodiversity"
df.base.2$Type<-"Biomass"
df.base.3<-rbind(df.base,df.base.2)


setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Results/Rasters/Biomass")
Biomass<-raster("AGB_2deg_for.tif")
plot(Biomass)


#convert the raster to points for plotting
map.AGB <- rasterToPoints(Biomass)

#Make the points a dataframe for ggplot
AGB.prior <- data.frame(map.AGB)
#Make appropriate column headings
colnames(AGB.prior) <- c("Long", "Lat", "Prior")
AGB.prior$Rank<-rank(AGB.prior$Prior)
AGB.prior$Type<-"Biomass"


Prior_comb<-rbind(df.prior,AGB.prior)
Prior_comb$Type<-as.factor(Prior_comb$Type)

head(Prior_comb)

Prior_comb$Continent<-ifelse(Prior_comb$Long>-20&Prior_comb$Long<70&Prior_comb$Lat>-35&Prior_comb$Lat<20,"Africa",NA)
Prior_comb$Continent<-ifelse(Prior_comb$Long>-120&Prior_comb$Long<(-30),"Americas",Prior_comb$Continent)
Prior_comb$Continent<-ifelse(Prior_comb$Long>65&Prior_comb$Long<170,"Asia & Australasia",Prior_comb$Continent)


Prior_comb$Continent2<-as.factor(Prior_comb$Continent)

Prior_comb2<-subset(Prior_comb,!is.na(Continent2))


ggplot(Prior_comb2,aes(x=Long,y=Lat))+geom_raster(aes(fill=Rank/max(Rank)))+facet_wrap(~Continent2,scales="free",ncol=1)+coord_equal()
ggsave("Priority_maps.png",width=6,height=8,units="in",dpi=400)


head(Prior_comb)
theme_set(theme_bw(base_size=12))
P_plot<-ggplot(df.base.3,aes(x=Long,y=Lat))+geom_raster(fill="light grey")+facet_wrap(~Type,ncol=2)
P_plot2<-P_plot+geom_raster(data=Prior_comb,aes(fill=Rank/max(Rank)))
P_plot3<-P_plot2+coord_equal()+scale_fill_gradient("Ranking",low="dark grey", high="dark green")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P_plot4<-P_plot3+ opts(axis.line=theme_blank(),axis.text.x=theme_blank(),
        axis.text.y=theme_blank(),axis.ticks=theme_blank(),
        axis.title.x=theme_blank(),
        axis.title.y=theme_blank(),
        panel.background=theme_blank(),panel.border=theme_blank(),panel.grid.major=theme_blank(),
        panel.grid.minor=theme_blank(),plot.background=theme_blank(),strip.background = theme_blank(),
        strip.text.x = theme_blank())

Af_prior<-P_plot4+xlim(-20,70)+ylim(-35,20)+theme(legend.position="none")+ theme(plot.margin= unit(c(0, 0, -1, 0), "cm"))
SA_prior<-P_plot4+xlim(-120,-30)+theme(legend.position="none")+ theme(plot.margin= unit(c(0, 0, -3, 0), "cm"))
As_prior<-P_plot4+xlim(65,170)+theme(legend.position="none")+ theme(plot.margin= unit(c(-2, 0, 0, 0), "cm"))

#Extract Legend
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend<- g_legend(P_plot4)

#save this
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures/")
png("Priority_maps.png",width=6,height=8,units="in",res=400)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.2, x = 0.375, y =0.1)
vp2 <- viewport(width = 0.75, height = 0.2, x = 0.375, y =0.35)
vp3 <- viewport(width = 0.75, height = 0.2, x = 0.375, y =0.6)
vpleg <- viewport(width = 0.25, height = 0.2, x = 0.9, y = 0.3)
print(Af_prior + opts(legend.position = "none"), vp = vp2)
print(As_prior + opts(legend.position = "none"), vp = vp1)
print(SA_prior + opts(legend.position = "none"), vp = vp3)
upViewport(0)
pushViewport(vpleg)
grid.draw(legend)
dev.off()



ggsave("Priority_maps.png",width=8,height=4,units="in",dpi=400)





#make a plot of the correlation between the two
Merge_Prior<-merge(AGB.prior,df.prior,by=c("Long","Lat"))
head(Merge_Prior)
cor(Merge_Prior$Prior.y,Merge_Prior$Prior.x)
Merge_Prior$Rank<-((Merge_Prior$Rank.x/1032)+(Merge_Prior$Rank.y/1056))/2
summary(Merge_Prior$Rank)

hist(Merge_Prior$Rank)

Merge_Prior$F_Rank<-(round(Merge_Prior$Rank,1))
Merge_Prior$F_Rank<-ifelse(Merge_Prior$F_Rank<=0.5,0.5,(Merge_Prior$F_Rank))
Merge_Prior$F_Rank<-as.factor(Merge_Prior$F_Rank)
levels(Merge_Prior$F_Rank)<-c("<0.5","0.6","0.7","0.8","0.9","1")


Merge_Prior$Rank_Diff<-((Merge_Prior$Rank.x/1032)-(Merge_Prior$Rank.y/1056))

hist(Merge_Prior$Rank_Diff)

theme_set(theme_bw(base_size=12))
P_plot<-ggplot(data=df.base,aes(x=Long,y=Lat))+geom_raster(fill="light grey")
P_plot2<-P_plot+geom_raster(data=Merge_Prior,aes(fill=F_Rank))+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P_plot3<-P_plot2+coord_equal()+scale_fill_brewer("Percentile rank\nfor both priorities",palette="Greens")+opts(axis.text.x=theme_blank(),
        axis.text.y=theme_blank(),axis.ticks=theme_blank(),
        axis.title.x=theme_blank(),
        axis.title.y=theme_blank())+ theme(plot.margin= unit(c(0, 0, 0, 0), "cm"))
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures/")
ggsave("Priority_both.png",width=10,height=3,units="in",dpi=400)

#differences in ranking
theme_set(theme_bw(base_size=12))
Diff_plot<-ggplot(data=df.base,aes(x=Long,y=Lat))+geom_raster(fill="grey")
Diff_plot2<-Diff_plot+geom_raster(data=Merge_Prior,aes(fill=Rank_Diff))+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Diff_plot3<-Diff_plot2+coord_equal()+scale_fill_gradient2("Different \nin rank between\ncarbon and\nbiodiversity\npriority")+opts(axis.text.x=theme_blank(),
        axis.text.y=theme_blank(),axis.ticks=theme_blank(),
        axis.title.x=theme_blank(),
        axis.title.y=theme_blank())
Diff_plot4<-Diff_plot3+ theme(plot.margin= unit(c(0, 0, 0, 0), "cm"))
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures/")
ggsave("Priority_both.png",width=10,height=3,units="in",dpi=400)

setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures/")
png(filename="Diff_Prior.png",width=8,height=4.5,units="in",res=400)
grid.arrange(Diff_plot4,P_plot3)
dev.off()


?grid.arrange

M1<-lm(plogis(Rank.y/max(Rank.y))~Rank.x,data=Merge_Prior)
plot(M1)

plot(plogis(Merge_Prior$Rank.y),Merge_Prior$Rank.x)

P_Pred<-data.frame(Rank.x=seq(min(Merge_Prior$Rank.x),max(Merge_Prior$Rank.x),by=1))
P_Pred$pred<-predict(M1,newdata=P_Pred)

mround <- function(x,base){ 
        base*round(x/base) 
} 

Mov_av<-data.frame(tapply(Merge_Prior$Rank.x/max(Merge_Prior$Rank.x),mround(Merge_Prior$Rank.y,25)/max(Merge_Prior$Rank.y),mean))
colnames(Mov_av)<-c("BD_rank")
Mov_av$C_rank<-as.numeric(rownames(Mov_av))


plot(Mov_av$C_rank,Mov_av$BD_rank)


library(ggplot2)
theme_set(theme_bw(base_size=10))
Prior_rel<-ggplot(data=Merge_Prior,aes(x=Rank.x/max(Rank.x),y=Rank.y/max(Rank.y)))+geom_point(shape=1,size=2)+geom_abline(size=1,lty=2,)
Prior_rel+geom_smooth(method="lm",se=F)
Prior_rel2<-Prior_rel+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Prior_rel2+coord_cartesian(xlim=c(0,1),ylim=c(0,1))+xlab("Biomass priority")+ylab("Bird biodiversity priority")
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/7. Spatial modelling of restoration/Rest_sp_mod/Figures/")
ggsave("Priority_relationship.png",width=6,height=4,units="in",dpi=400)



