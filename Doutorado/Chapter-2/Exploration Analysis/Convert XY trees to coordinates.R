#Convert XY trees to coordinates
#Blowdown Data (AREA-1)
#Eduardo Q Marques 01-06-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(fmsb)

#Load data
setwd('C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento')

df = read.csv("blowdown_full_update_2021.csv", sep = ",")

# ===============================================================================================


















#Interative Map ===============================================================================
library(leaflet)

eqm2 <- colorFactor(c("#003024","#fee08b","#d73027","#00ff00","#228c22"), domain = values(class11),
                    na.color = "transparent")

leaflet() %>% 
  addTiles() %>% 
  addMiniMap(position = "bottomleft") %>%
  addProviderTiles("Esri.WorldImagery") %>% 
  addRasterImage(class11, colors = eqm2, opacity = 0.8) %>% 
  addPolygons(data = area1, color = "#000000",
              weight = 1, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = area1@data$Nome, label = area1@data$Nome) %>% 
  addLegend(pal = eqm2, 
            values = values(class11), 
            title = "Classes",
            opacity = 1)