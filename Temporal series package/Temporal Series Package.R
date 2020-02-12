###############################################
#          TEMPORAL SERIES PACKAGE            #
#          => A Temporal scale <==            #
# By: Eduardo Q Marques   10-02-2020          #
###############################################

#This script is for construction and test my oun package to work with temporal series and satelitte images

#Packages requiries ====================================================================
library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)

#Functions =============================================================================
#Extract point temporal data (x is raster, y is point file)
time_point = function(x,y){
  ext <- raster::extract(x, y)
  a <- melt(ext)
  a_md = a %>%
    group_by(date, parcela) %>% 
    summarise(index = median(index))
}








#To calculate median per plot
data_time_median = function(x,y){
  ext <- raster::extract(x, y)
  a <- melt(ext)
  a_md = a %>%
    group_by(date, parcela) %>% 
    summarise(index = median(index))
}
