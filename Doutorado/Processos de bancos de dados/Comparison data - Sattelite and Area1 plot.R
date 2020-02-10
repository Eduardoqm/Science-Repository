#Plots to comparation Insdex data and Area1 data

#Eduardo Q Marques 06-02-2020
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)

#Data bank
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1')

#Lansat
landsat = read.csv("Landast_indexs_median by plot.csv")
landsat = landsat %>% 
  unite(col = "id", c("parcela", "date", "dist"), sep = '_')

#Hyperion
hyper = read.csv("Hyperion_indexs_median by plot_2.csv")
hyper = hyper %>% 
  unite(col = "id", c("parcela", "date", "dist"), sep = '_')

#All satellite data
satellite = full_join(landsat, hyper, by = "id")
satellite = satellite %>%
  separate(col = "id", c("parcela", "date", "dist"), sep = '_')

#Process to plot
sat = melt(satellite)

sat_edge = sat %>%
  filter(dist == "borda")

sat_core = sat %>%
  filter(dist == "nucleo")

#Plot data
ggplot(sat_edge, aes(date, value, color = variable))+
  geom_line(aes(group=variable), size = 1)+
  geom_point()
  











