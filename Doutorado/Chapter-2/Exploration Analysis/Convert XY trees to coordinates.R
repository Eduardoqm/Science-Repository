#Convert XY trees to coordinates
#Blowdown Data (AREA-1)
#Eduardo Q Marques 01-06-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(fmsb)

#Load data
setwd('C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento')

df = read.csv("blowdown_full_update_2021_B.csv", sep = ",")

df2 = df[,c(1,7,8,9,18,19,20,21,22,23,27,28)]
df2 = df2[,c(-4,-6,-7)]

# ===============================================================================================

#OBS:
#1-metragem e a distancia do canto nordeste da parcela indo de leste para oeste
#2-lodist e a distancia em metros da linha no sentido leste ou oeste
#3-nsdist com o transecto e em metros referente ao norte ou sul do transecto
#4-nsdits sem o transsecto e em metros referente a borda da parcela

#Linhas de 1 a 31, a cada 100 metros


#Posso separar os metodos nas etapas de transformacao para as classes:

#5-10

#10-20

#40










#Interative Map ===============================================================================
library(leaflet)
library(raster)

leaflet() %>% 
  addTiles() %>% 
  #addMiniMap(position = "bottomleft") %>%
  addProviderTiles("Esri.WorldImagery")
  #addRasterImage(class11, colors = eqm2, opacity = 0.8) %>% 
  #addPolygons(data = area1, color = "#000000",
   #           weight = 1, fillOpacity = 0,
    #          highlightOptions = highlightOptions(color = "white", weight = 2,
     #                                             bringToFront = TRUE),
      #        popup = area1@data$Nome, label = area1@data$Nome)
