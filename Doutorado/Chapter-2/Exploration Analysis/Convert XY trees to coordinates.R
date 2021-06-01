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
#1-metragem e a distancia do canto da parcela leste para oeste, linha 1 (0m) para linha 31 (1500m)
#2-lodist e a distancia em metros da linha no sentido leste ou oeste
#3-nsdist com o transecto e em metros referente ao norte ou sul do transecto
#4-nsdits sem o transsecto e em metros referente a borda da parcela

#Linhas de 1 a 31, a cada 50 metros (X)
#Transectos de A a U, a cada 50 metros (Y)
#Execoes A, AA, AB, B:
#A = 0m
#AA = 5m?
#AB = 25m
#B = 50m

#X = 0:1500
#Y = 0:1000

#Posso separar os metodos nas etapas de transformacao para as classes:

#5-10 tem nsdist(Y), transecto(Y) e metragem(X)

#10-20 tem transecto(Y) e metragem(X)

#40 tem linha(X), nsdist(Y) e lodist

#Transformation ===============================================================================
#Part 1 - Transect to meter
df2$transecto = as.character(df2$transecto)
df2$trasc_m[df2$transecto == "A"] <- 0
df2$trasc_m[df2$transecto == "AA"] <- 5
df2$trasc_m[df2$transecto == "AB"] <- 25
df2$trasc_m[df2$transecto == "B"] <- 50
df2$trasc_m[df2$transecto == "C"] <- 100
df2$trasc_m[df2$transecto == "D"] <- 150
df2$trasc_m[df2$transecto == "E"] <- 200
df2$trasc_m[df2$transecto == "F"] <- 250
df2$trasc_m[df2$transecto == "G"] <- 300
df2$trasc_m[df2$transecto == "H"] <- 350
df2$trasc_m[df2$transecto == "I"] <- 400
df2$trasc_m[df2$transecto == "J"] <- 450
df2$trasc_m[df2$transecto == "K"] <- 500
df2$trasc_m[df2$transecto == "L"] <- 550
df2$trasc_m[df2$transecto == "M"] <- 600
df2$trasc_m[df2$transecto == "N"] <- 650
df2$trasc_m[df2$transecto == "O"] <- 700
df2$trasc_m[df2$transecto == "P"] <- 750
df2$trasc_m[df2$transecto == "Q"] <- 800
df2$trasc_m[df2$transecto == "R"] <- 850
df2$trasc_m[df2$transecto == "S"] <- 900
df2$trasc_m[df2$transecto == "T"] <- 950
df2$trasc_m[df2$transecto == "U"] <- 1000
#df2$trasc_m[df2$transecto == "NA"] <- c("eqm")
df2$trasc_m[is.na(df2$transecto)] <- c(df2$nsdist)








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
