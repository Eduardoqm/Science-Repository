###############################################
#          TEMPORAL SERIES PACKAGE            #
#          => A Temporal scale <==            #
# By: Eduardo Q Marques   10-02-2020          #
###############################################

#This script is for construction and test my oun package to work with temporal series and satelitte images

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)


#Data bank to test======================================================================
ndvi <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package/NDVI", pattern = ".tif$", full.names=TRUE,recursive=TRUE))


#Polygon to get values
polygons <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package",layer="polygons")

polygon <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package",layer="polygon")

points <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package",layer="points")

point <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package",layer="point")

lines <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package",layer="lines")

line <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package",layer="line")



area1 = spTransform(area1, crs(ndvi))


#Functions =============================================================================
#To calculate median per plot
data_time_median = function(x,y){
  ext <- raster::extract(x, y)
  a <- melt(ext)
  a_md = a %>%
    group_by(date, parcela) %>% 
    summarise(index = median(index))
}
