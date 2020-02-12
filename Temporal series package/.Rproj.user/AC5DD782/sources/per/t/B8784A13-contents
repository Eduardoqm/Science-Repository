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

#Functions
# With Points Vectors ==================================================================
#Extract point temporal data (x is raster, y is point file)
df_point = function(x,y){
  ext <- raster::extract(x, y)
  ext <- melt(ext)
  ext <- as.data.frame(ext)
}

#Extract points temporal data (x is raster, y is point file, z is method)
df_points = function(x,y,z){
  ext <- raster::extract(x, y)
  ext <- melt(ext)
  ext <- as.data.frame(ext)
    if (z == "median") {
      ext_md = ext %>%
        group_by(Var2) %>% 
        summarise(value = median(value))
    } else if (z == "mean") {
      ext_md = ext %>%
        group_by(Var2) %>% 
        summarise(value = mean(value))
    } else if(z == "max"){
      ext_md = ext %>%
    group_by(Var2) %>% 
    summarise(value = max(value))
    } else if(z == "min"){
      ext_md = ext %>%
        group_by(Var2) %>% 
        summarise(value = min(value))
    }else if(z == "sd"){
      ext_md = ext %>%
        group_by(Var2) %>% 
        summarise(value = sd(value))
    }else
      print("Use a valid method to calculate!")
}


#Extract point temporal data and plot(x is raster, y is point file)
gg_point = function(x,y){
  ext <- raster::extract(x, y)
  ext <- melt(ext)
  ext <- as.data.frame(ext)
  ggplot(time_point, aes(Var2, value))+
    geom_line(aes(group = 1), size = 1)+
    labs(x="Time",y="Value")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90))
}




