#---------------------------------------------------------------------------
# Precipitation (ERA5 Land) vs Wind Gust (ERA5 Single Levels) Area-1 Point
# 
# DATA FRAME generate
#
# Eduardo Q Marques 02-08-2022
#----------------------------------------------------------------------------

library(raster)
library(rgdal)
library(sf)
library(ncdf4)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(viridis)
library(extRemes)
library(boot)

#rasterOptions(tmpdir='/net/so4/landclim/equeiroz/tmp/') #Temporary folder on server

#Load data ---------------------------------------------------------------------
ws = brick("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/UFZ Data/ERA5_Single_Levels/ERA5_SL-Wind_Gust-2012_2020.nc") #Wind Gust from ERA5 Single Levels

ppt = brick("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/UFZ Data/ERA5_land/Recurrent_Precipitation_ERA5-Land_2012_2020.nc") # Recurrent Precipitation from ERA5 Land

tp = brick("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/UFZ Data/ERA5_land/Total_Precipitation_ERA5-Land_2012_2020.nc") #Total Precipitation from ERA5 Land

ponto = readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/UFZ Data/Shapes", layer = "Area1_ponto") #Area-1 point
tang = readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/UFZ Data/Shapes", layer = "Tanguro_limites") #Area-1 point
xing = readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/UFZ Data/Shapes", layer = "Xingu_MT") #Area-1 point

#Names em ppt stack
ppt@data@names = tp@data@names


#Plot to verify data -----------------------------------------------------------
plot(ppt[[10]])
plot(xing, add = T)
plot(tang, add = T)
plot(ponto, add = T)

#Extract info from ERA5 with buffer --------------------------------------------
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

#df2 = df[,c(1,2,6,3,4)]; colnames(df2) = c("date", "ws", "ppt", "year", "month")
#df$m = as.character(df$m)
#df = df %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate

setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/UFZ Data/DataFrames")
write.csv(df, file = "ERA5_Prec_WG_Area1_Point.csv", sep = ",", row.names = F)











