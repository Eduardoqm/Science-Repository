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
gg_point = function(x,y,g){
  ext <- raster::extract(x, y)
  ext <- melt(ext)
  ext <- as.data.frame(ext)
  if (g == "line") {
    ggplot(ext, aes(Var2, value))+
      geom_line(aes(group = 1), size = 1)+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  } else if (g == "point") {
    ggplot(ext, aes(Var2, value))+
      geom_point(aes(group = 1), size = 1)+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  } else if(g == "boxplot"){
    ggplot(ext, aes(Var2, value))+
      geom_boxplot()+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  } else if(g == "bar"){
    ggplot(ext, aes(Var2, value))+
      geom_bar(stat = "identity")+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  }else if(g == "area"){
    ggplot(ext, aes(Var2, value))+
      geom_area(aes(group = 1))+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  }else
    print("Use a valid method to plot!")
}


#Extract points temporal data  and plot(x is raster, y is point file, z is method, g is kind of plot)
gg_points = function(x,y,z,g){
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
  
  if (g == "line") {
    ggplot(ext_md, aes(Var2, value, fill = value))+
      geom_line(aes(group = 1), size = 1)+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  } else if (g == "point") {
    ggplot(ext_md, aes(Var2, value))+
      geom_point(aes(group = 1), size = 1)+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  } else if(g == "boxplot"){
    ggplot(ext_md, aes(Var2, value))+
      geom_boxplot()+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  } else if(g == "bar"){
    ggplot(ext_md, aes(Var2, value))+
      geom_bar(stat = "identity")+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  }else if(g == "area"){
    ggplot(ext_md, aes(Var2, value))+
      geom_area(aes(group = 1))+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  }else
    print("Use a valid method to plot!")
}

# With Polygons Vectors ==================================================================
#Extract polygons temporal data (x is raster, y is polygons file, z is method)
df_poly = function(x,y,z){
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


#Extract polygons temporal data  and plot(x is raster, y is polygons file, z is method, g is kind of plot)
gg_poly = function(x,y,z,g){
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
  
  if (g == "line") {
    ggplot(ext_md, aes(Var2, value, fill = value))+
      geom_line(aes(group = 1), size = 1)+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  } else if (g == "point") {
    ggplot(ext_md, aes(Var2, value))+
      geom_point(aes(group = 1), size = 1)+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  } else if(g == "boxplot"){
    ggplot(ext_md, aes(Var2, value))+
      geom_boxplot()+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  } else if(g == "bar"){
    ggplot(ext_md, aes(Var2, value))+
      geom_bar(stat = "identity")+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  }else if(g == "area"){
    ggplot(ext_md, aes(Var2, value))+
      geom_area(aes(group = 1))+
      labs(x="Time",y="Value")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  }else
    print("Use a valid method to plot!")
}

