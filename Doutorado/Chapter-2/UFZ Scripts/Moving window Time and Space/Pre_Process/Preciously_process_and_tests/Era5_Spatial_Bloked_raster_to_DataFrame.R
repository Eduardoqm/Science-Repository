#---------------------------------------------------------------------------
# Precipitation vs Wind Gust Area-1 Point (Saptial Bloked)
# 
# DATA FRAME generate
#
# Eduardo Q Marques 10-08-2022
#----------------------------------------------------------------------------

library(raster)
#Essential Functions ----------------------------------------------------------------------
#Function to plot data in EVE server
plotx = function(w) {
    setwd("/home/queirozm/eqm_eth_ufz/Temp_maps")
    png(paste0(deparse(substitute(w)), ".png"))
    plot(w)
    dev.off()
}

#Load data ---------------------------------------------------------------------
setwd("/home/queirozm/eqm_eth_ufz/Data")

ws = brick("ERA5_SL/ERA_SL_WG_block_7pixels.nc") #Wind Gust Bloked
wg = brick("ERA5_SL/ERA5_Single_Levels-Wind_Gust-2012_2020.nc") #Wind Gust from ERA5 Single Levels
ws@data@names = wg@data@names #Names em wg stack

ppt = brick("ERA5_land/ERA_Land_Prec_block_7pixels.nc") # Recurrent Precipitation from ERA5 Land
tp = brick("ERA5_land/Total_Precipitation_ERA5-Land_2012_2020.nc") #Total Precipitation from ERA5 Land
ppt@data@names = tp@data@names #Names em ppt stack

library(rgdal)
ponto = readOGR(dsn = "Shapes", layer = "Area1_ponto") #Area-1 point
tang = readOGR(dsn = "Shapes", layer = "Tanguro_limites") #Area-1 point
xing = readOGR(dsn = "Shapes", layer = "Xingu_MT") #Area-1 point

#Plot to verify data -----------------------------------------------------------
plotx(ppt)
plotx(ws)

#Extract info from ERA5 with buffer --------------------------------------------
library(tidyverse)
library(reshape2)

#Wind Speed
wsdf <- raster::extract(ws, ponto)
wsdf = melt(wsdf)
wsdf = wsdf[,-1]

colnames(wsdf) = c("Date", "Wind")
wsdf$Date = substr(wsdf$Date, 2, 14)

#Precipitation
pptdf <- raster::extract(ppt, ponto)
pptdf = melt(pptdf)
pptdf = pptdf[,-1]

colnames(pptdf) = c("Date", "Prec")
pptdf$Date = substr(pptdf$Date, 2, 14)

#Cleannig recalculate erros from precipitation
pptdf = pptdf[c(-1,-2),] #First and second data from stack is not accurate

#First hour of day is not right calculated (deleting)
pptdf$time = substr(pptdf$Date, 12, 13)
pptdf = pptdf %>% filter(time != "01")

pptdf = pptdf[,-3]

#Summarize ---------------------------------------------------------------------
#ERA5 Land Wind Speed
wsdf$y = as.numeric(substr(wsdf$Date, 1, 4))
wsdf$m = as.numeric(substr(wsdf$Date, 6, 7))
wsdf$d = as.numeric(substr(wsdf$Date, 9, 10))

#ERA5 Land Total Precipitation
pptdf$y = as.numeric(substr(pptdf$Date, 1, 4))
pptdf$m = as.numeric(substr(pptdf$Date, 6, 7))
pptdf$d = as.numeric(substr(pptdf$Date, 9, 10))

#Unite and Correlate data ------------------------------------------------------
df = full_join(wsdf, pptdf, id = "Date")

#Save file ----------------------------------------------------------------------
setwd("/home/queirozm/eqm_eth_ufz/Data/DataFrames/ERA5_Area1")
write.csv(df, file = "ERA5_Prec_WG_Area1_Point_7p.csv", sep = ",", row.names = F)