##################################################################################################
# Please before run the code install the packages!


options(java.parameters = "-Xmx4g" )

library(XLConnect)
library(rgdal)
library(RColorBrewer)
library(lubridate)
library(doBy)
library(dplyr)
library(trend)
library(classInt)
library(mapview)
library(leaflet)
library(htmlwidgets)



month2season=function(x) {
  res=NA
  if ( (x==1) || (x==2) ||(x==12)) {res="inv"};
  if ( (x==3) || (x==4) || (x==5) ) {res="prim"};
  if ( (x==6) || (x==7) ||(x==8)) {res="est"};
  if ( (x==9) || (x==10) ||(x==11)) {res="aut"};
  return(res);
  
}

##################################################################################################################
# Read ISTAT GIS data and ordering by name of province.

ProvITA=readRDS("ProvITA_CS_ispra.rds")
ProvITA=ProvITA[order(ProvITA@data$DEN_CMPRO),]


##################################################################################################
# Read italian province data 

prov_stats=read.csv("prov_stats.csv",stringsAsFactors = F)

ProvITA@data=cbind(ProvITA@data,prov_stats)

label_prov=gsub(" ",".",gsub("-",".",prov_stats$Nprov))


##################################################################################################
# Read tmean daily E.OBS data from ECA&D 

tg_ita=brick("tg_ita_eobs_1950_2015.nc",lvar=1)

mean_tg_prov=t(raster::extract(tg_ita,ProvITA_CS_ispra,fun=mean,na.rm = T))

mean_tg_1980_2015=as.data.frame(tail(mean_tg_prov,426))

names(mean_tg_1980_2015)=gsub("'","",label_prov)

saveRDS(mean_tg_1980_2015,"mean_tg_1980_2015_prov.rds")

##################################################################################################
# Manage data

mean_tg_1980_2015_prov=readRDS("mean_tg_1980_2015_prov.rds")

mean_tg_1980_2015_prov$date=as.Date(ymd(gsub("^X","",row.names(mean_tg_1980_2015_prov))))
row.names(mean_tg_1980_2015_prov)=NULL

mean_tg_1980_2015_prov$year=year(mean_tg_1980_2015_prov$date)
mean_tg_1980_2015_prov$month=month(mean_tg_1980_2015_prov$date)
mean_tg_1980_2015_prov$stag=sapply(mean_tg_1980_2015_prov$month,month2season)


##################################################################################################
# Seasonal extraction


mean_tg_1980_2015_prov_inv=mean_tg_1980_2015_prov[grep("inv",mean_tg_1980_2015_prov$stag),]
mean_tg_1980_2015_prov_est=mean_tg_1980_2015_prov[grep("est",mean_tg_1980_2015_prov$stag),]
mean_tg_1980_2015_prov_est$year=as.factor(mean_tg_1980_2015_prov_est$year)
mean_tg_1980_2015_prov_est$stag=as.factor(mean_tg_1980_2015_prov_est$stag)

mean_tg_1980_2015_prov_est[,49]=mean_tg_1980_2015_prov_est[,75] # correction of Livorno data with Pisa for data inconsistency

############################################################################################################################
# calculate climatic trends

res=list()
res_trend_s=list()
res_trend_s_up=list()
res_trend_s_lo=list()

res_mean=list()

for ( i in 1:110 ) {
                      temp=tapply(mean_tg_1980_2015_prov_est[,i], mean_tg_1980_2015_prov_est$year,mean)
                      res[[i]]=as.numeric(temp)
                      temp_ts=ts(as.numeric(temp), frequency = 1, start = 1980)
                      temp_trend=sens.slope(temp_ts,0.95)
                      res_trend_s[[i]]=temp_trend$b.sen
                      res_trend_s_up[[i]]=temp_trend$b.sen.up
                      res_trend_s_lo[[i]]=temp_trend$b.sen.lo
                      res_mean[[i]]=mean(as.numeric(temp),na.rm=T)
                      
                   }

res_temp=data.frame(Mean_summer=unlist(res_mean),trend_s=unlist(res_trend_s),trend_s_up=unlist(res_trend_s_up),trend_s_lo=unlist(res_trend_s_lo))

ProvITA@data=cbind(ProvITA@data,res_temp)

##########################################################################################
# Normalize layers

ProvITA$Cs_15P_scaled=as.numeric(scale(ProvITA$Cs_15P,center=min(ProvITA$Cs_15P),scale=diff(range(ProvITA$Cs_15P))))
ProvITA$pop2016_scaled=as.numeric(scale(ProvITA$pop2016,center=min(ProvITA$pop2016),scale=diff(range(ProvITA$pop2016))))
ProvITA$trend_scaled=as.numeric(scale(ProvITA$trend_s,center=min(ProvITA$trend_s),scale=diff(range(ProvITA$trend_s))))
ProvITA$Mean_summer_scaled=as.numeric(scale(ProvITA$Mean_summer,center=min(ProvITA$Mean_summer),scale=diff(range(ProvITA$Mean_summer))))

##########################################################################################
# Calculate Summer Heat Risk Index

ProvITA$riskvalue=(0.5*ProvITA$trend_scaled)+(0.5*ProvITA$pop2016_scaled+0.5*ProvITA$Cs_15P_scaled)*0.5

################################################################################################################à
# Risk class calculation 

class_risk=classIntervals(ProvITA$riskvalue, n =5,style="fixed",fixedBreaks=c(0,0.20,0.40,0.60,0.80,1))
ProvITA$riskclass=cut(ProvITA$riskvalue, breaks = class_risk$brks, labels=c("Very Low", "Low","Moderate","High","Very High"))


writeWorksheetToFile("Analisi_rischio.xls",ProvITA@data,sheet="Paraemteri Rischio")

saveRDS(ProvITA,"ProvITA_CS_par.rds")


