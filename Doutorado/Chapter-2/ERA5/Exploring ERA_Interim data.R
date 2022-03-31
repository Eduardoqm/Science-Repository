#----------------------------
#Exploring ERA_Interim data
#----------------------------
#Eduardo Q Marques 14-03-2022
#eduardobio2009@gmail.com
#----------------------------

library(raster)
library(rasterVis)
library(rgdal)
library(viridis)

setwd("~/Data/ERA_Interim")
dir()

wg = brick("Wind_gust_ERA_Interim_Feb_2019.nc")
tang = readOGR(dsn = "~/Data", layer="Tanguro_limites")

#tang = spTransform(tang, crs(wg[[1]]))

#plot(tang) wind gust from 3th Feb 2019
levelplot(wg[[16:24]], margin = F, col.regions = viridis(100))
levelplot(wg[[23]], margin = F, col.regions = viridis(100))
