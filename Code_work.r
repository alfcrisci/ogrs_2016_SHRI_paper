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

##################################################################################################
setwd("/home/alf/Scrivania/alf_github/ogrs_2016_papers/morabito_et_al")

morab_pal_VGARV=c("#00CC00","#FFFF00","#FF7519","#FF0000","#FF00FF")
morabcols <- colorRampPalette( c(morab_pal_VGARV))(5)




ProvITA_CS_par=readRDS("ProvITA_CS_par.rds")

rgdal::writeOGR(ProvITA_CS_par, ".", "ProvITA_CS_par", driver="ESRI Shapefile",overwrite_layer = T)

writeWorksheetToFile(paste0("ProvITA_CS_data",".xls"),ProvITA_CS_par,sheet="Rischio")

mapviewOptions(basemaps = c( "CartoDB.Positron","OpenStreetMap","Esri.WorldImagery"),
               raster.palette = colorRampPalette(brewer.pal(9, "Greys")),
               vector.palette = morabcols,
               na.color = "grey",
               layers.control.pos = "topright")


risk=mapview(ProvITA_CS_par,zcol = "riskclass",color=morabcols,legend=T)
risk_map=risk@map %>% 
  addLegend("bottomright", colors = morabcols,labels=c("Very Low", "Low","Moderate","High","Very High"),title="Summer Heat Risk")



palCs_15P=c<- colorQuantile("YlOrRd", ProvITA_CS_par@data$Cs_15P, n = 5)

riskCs_15P=mapview(ProvITA_CS_par,zcol = "Cs_15P",color =palCs_15P, legend=T)
riskCs_15P@map %>% 
  addLegend("bottomright", pal = palCs_15P, values = ProvITA_CS_par@data$Cs_15P,
            title = "Percentage Soil Consumption",
            opacity = 1
  )

ProvITA_CS_par@data$trend_s=ProvITA_CS_par@data$trend_s*10
trendcols <- colorRampPalette(rev(brewer.pal(9, "RdYlBu")))(110)
paltrend <- colorNumeric(
  palette = trendcols,
  domain = ProvITA_CS_par@data$trend_s
)


risktrend_s=mapview(ProvITA_CS_par,zcol = "trend_s",color =paltrend(ProvITA_CS_par@data$trend_s), legend=T)
risktrend_s@map %>% 
  addLegend("topright", pal = paltrend, values = ProvITA_CS_par@data$trend_s,
            title = " Summer Tair Trend \r deg/10Yr 1980-2015",
            opacity = 1
  )


ProvITA_CS_par@data$pop2016=ProvITA_CS_par@data$pop2016*1

morabcols_2 <- colorRampPalette(brewer.pal(5, "YlGnBu"))(10)

pal <- colorNumeric(
  palette = morabcols_2,
  domain = ProvITA_CS_par@data$pop2016
)


riskpop2016=mapview(ProvITA_CS_par,zcol ="pop2016",color=morabcols_2,legend=TRUE)


riskpop2016@map %>% 
  addLegend("topright", pal = pal, values = ProvITA_CS_par@data$pop2016,
            title = "Population ISTAT 01-01-2016",opacity = 1
  )

#########################################################################à
# [1] "COD_REG"                         "COD_CM"                          "COD_PRO"                        
# [4] "DEN_CMPRO"                       "PROVINCIA"                       "SIGLA"                          
# [7] "FLAG_CMPRO"                      "Shape_Leng"                      "Shape_Area"                     
# [10] "cod_prov"                        "provincia"                       "Cs0_12ha"                       
# [13] "Cs1_12ha"                        "Ca2_12"                          "Cs_12P"                         
# [16] "Cs0_15ha"                        "Cs1_15ha"                        "Cs2_15P"                        
# [19] "Cs_15P"                          "D_15.12"                         "I_CS_P"                         
# [22] "ProvITA_CS_ispra.data.DEN_CMPRO" "Nprov"                           "Sreg"                           
# [25] "pop2016"                         "Supkmq"                          "density_pop"                    
# [28] "Ncom"                            "Mean_summer"                     "trend_s"                        
# [31] "trend_s_up"                      "trend_s_lo"                      "Mean_summer_scaled"             
# [34] "Cs_15P_scaled"                   "pop2016_scaled"                  "trend_scaled"                   
# [37] "riskvalue"                       "riskclass"                      
##################################################################################################à
