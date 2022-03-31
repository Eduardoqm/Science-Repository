#------------------------------
# CHIRPS raster to data frame
#------------------------------
# Eduardo Q Marques 28-03-2022
# eduardobio2009@gmail.com
#------------------------------

library(raster)
library(rasterVis)
library(rgdal)
library(viridis)
library(tidyverse)
library(reshape2)

#Load data ---------------------------------------------------------------------
setwd("~/Data/CHIRPS")
lista = list.files(path = "~/Data/CHIRPS")

#Converting to data frame ------------------------------------------------------
#2012 --------------------------------------------------------------------------
img = brick(lista[[1]])
df = as.data.frame(img[[1]], xy = T)
df$date = 1
df$year = 2012
colnames(df) = c("x","y","prec","date","year")

for (x in 2:length(img@data@names)) {
  print(length(img@data@names) - x)
  dfb = as.data.frame(img[[x]], xy = T)
  dfb$date = x
  dfb$year = 2012
  colnames(dfb) = c("x","y","prec","date","year")
  df = rbind(df, dfb)
}
df12 = df

#2013 --------------------------------------------------------------------------
img = brick(lista[[2]])
df = as.data.frame(img[[1]], xy = T)
df$date = 1
df$year = 2013
colnames(df) = c("x","y","prec","date","year")

for (x in 2:length(img@data@names)) {
  print(length(img@data@names) - x)
  dfb = as.data.frame(img[[x]], xy = T)
  dfb$date = x
  dfb$year = 2013
  colnames(dfb) = c("x","y","prec","date","year")
  df = rbind(df, dfb)
}
df13 = df

#2014 --------------------------------------------------------------------------
img = brick(lista[[3]])
df = as.data.frame(img[[1]], xy = T)
df$date = 1
df$year = 2014
colnames(df) = c("x","y","prec","date","year")

for (x in 2:length(img@data@names)) {
  print(length(img@data@names) - x)
  dfb = as.data.frame(img[[x]], xy = T)
  dfb$date = x
  dfb$year = 2014
  colnames(dfb) = c("x","y","prec","date","year")
  df = rbind(df, dfb)
}
df14 = df

#2015 --------------------------------------------------------------------------
img = brick(lista[[4]])
df = as.data.frame(img[[1]], xy = T)
df$date = 1
df$year = 2015
colnames(df) = c("x","y","prec","date","year")

for (x in 2:length(img@data@names)) {
  print(length(img@data@names) - x)
  dfb = as.data.frame(img[[x]], xy = T)
  dfb$date = x
  dfb$year = 2015
  colnames(dfb) = c("x","y","prec","date","year")
  df = rbind(df, dfb)
}
df15 = df

#2016 --------------------------------------------------------------------------
img = brick(lista[[5]])
df = as.data.frame(img[[1]], xy = T)
df$date = 1
df$year = 2016
colnames(df) = c("x","y","prec","date","year")

for (x in 2:length(img@data@names)) {
  print(length(img@data@names) - x)
  dfb = as.data.frame(img[[x]], xy = T)
  dfb$date = x
  dfb$year = 2016
  colnames(dfb) = c("x","y","prec","date","year")
  df = rbind(df, dfb)
}
df16 = df

#2017 --------------------------------------------------------------------------
img = brick(lista[[6]])
df = as.data.frame(img[[1]], xy = T)
df$date = 1
df$year = 2017
colnames(df) = c("x","y","prec","date","year")

for (x in 2:length(img@data@names)) {
  print(length(img@data@names) - x)
  dfb = as.data.frame(img[[x]], xy = T)
  dfb$date = x
  dfb$year = 2017
  colnames(dfb) = c("x","y","prec","date","year")
  df = rbind(df, dfb)
}
df17 = df

#2018 --------------------------------------------------------------------------
img = brick(lista[[7]])
df = as.data.frame(img[[1]], xy = T)
df$date = 1
df$year = 2018
colnames(df) = c("x","y","prec","date","year")

for (x in 2:length(img@data@names)) {
  print(length(img@data@names) - x)
  dfb = as.data.frame(img[[x]], xy = T)
  dfb$date = x
  dfb$year = 2018
  colnames(dfb) = c("x","y","prec","date","year")
  df = rbind(df, dfb)
}
df18 = df

#2019 --------------------------------------------------------------------------
img = brick(lista[[8]])
df = as.data.frame(img[[1]], xy = T)
df$date = 1
df$year = 2019
colnames(df) = c("x","y","prec","date","year")

for (x in 2:length(img@data@names)) {
  print(length(img@data@names) - x)
  dfb = as.data.frame(img[[x]], xy = T)
  dfb$date = x
  dfb$year = 2019
  colnames(dfb) = c("x","y","prec","date","year")
  df = rbind(df, dfb)
}
df19 = df

#2020 --------------------------------------------------------------------------
img = brick(lista[[9]])
df = as.data.frame(img[[1]], xy = T)
df$date = 1
df$year = 2020
colnames(df) = c("x","y","prec","date","year")

for (x in 2:length(img@data@names)) {
  print(length(img@data@names) - x)
  dfb = as.data.frame(img[[x]], xy = T)
  dfb$date = x
  dfb$year = 2020
  colnames(dfb) = c("x","y","prec","date","year")
  df = rbind(df, dfb)
}
df20 = df

#Join and save dataframe -------------------------------------------------------
df_all = rbind(df12, df13, df14, df15, df16, df17, df18, df19, df20)

setwd("~/Data/Tanguro Tower data")
#write.csv(df_all, "CHIRPS_2012-2020.csv", sep = ",", row.names = F)

#Testing data ------------------------------------------------------------------
df2 = df_all %>% 
  na.omit() %>% 
  group_by(date, year) %>% 
  summarise(prec = max(prec))

library(ggplot2)

ggplot(df2, aes(x = date,y = prec))+
  geom_line()+
  facet_wrap(~year)





