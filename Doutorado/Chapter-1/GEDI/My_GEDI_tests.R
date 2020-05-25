#My script to test GIDE

#Eduardo Q Marques 18-03-2020

#Edu LAS Exploration
library(lidR)
library(plot3D)
library(ggplot2)
library(plotly)
library(rayshader)
library(rasterVis)
library(viridis)

setwd('C:\\Users\\Eduardo Q Marques\\Documents\\test_gedi')
las_amazon<-readLAS("Amazon.las")
las_savanna<-readLAS("Savanna.las" )
#las_amazon<-readLAS(lasfile_amazon)
#las_savanna<-readLAS(lasfile_savanna)

plot(las_amazon)
amaz = as.data.frame(las_amazon@data)
sav = as.data.frame(las_savanna@data)

#Tryng plotly 3D plots
plot_ly(x = amaz$X, y = amaz$Y, z = amaz$Z, color = amaz$Z, size = I(5))

plot_ly(x = sav$X, y = sav$Y, z = sav$Z, color = sav$Z, size = I(5))
