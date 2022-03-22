#------------------------------
# CHIRPS raster to data frame
#------------------------------
# Eduardo Q Marques 22-03-2022
# eduardobio2009@gmail.com
#------------------------------

library(raster)
library(rasterVis)
library(rgdal)
library(tidyverse)
library(reshape2)

#Load data ----------------------------------------------------------------------------
setwd("~/Research/Doutorado/Banco de Dados Tanguro/CHIRPS")
img = brick("CHIRPS_daily_year_2019.tif")

tang = readOGR(dsn = "~/Research/Doutorado/Banco de Dados Tanguro/Shapes/Tanguro farm", layer = "Tanguro_limites")

#Exploration info ---------------------------------------------------------------------
#Exploration info ---------------------------------------------------------------------
plot(img[[33]])
plot(tang, add=T)

levelplot(img[[31:36]])

df = as.data.frame(img[[1]], xy = T)
df$date = 1
df$year = 2019
colnames(df) = c("x","y","prec","date","year")

View(df)

for (x in 2:length(img@data@names)) {
  print(length(img@data@names) - x)
  #plot(img[[x]])
  dfb = as.data.frame(img[[x]], xy = T)
  dfb$date = x
  dfb$year = 2019
  colnames(dfb) = c("x","y","prec","date","year")
  df = rbind(df, dfb)
}




library(ggplot2)

ggplot(df, aes(x,y, fill = prec))+
  geom_raster()


