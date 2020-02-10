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

hyper = hyper[,c(-12)]

hyper = hyper %>% 
  unite(col = "id", c("parcela", "date", "dist"), sep = '_')

#All satellite data
satellite = full_join(landsat, hyper, by = "id")
satellite = satellite %>%
  separate(col = "id", c("parcela", "date", "dist"), sep = '_')

#Process to plot
sat = melt(satellite)

sat_edge_crt = sat %>%
  filter(dist == "borda", parcela == "controle")

sat_edge_b3y = sat %>%
  filter(dist == "borda", parcela == "b3yr")

sat_edge_b1y = sat %>%
  filter(dist == "borda", parcela == "b1yr")


sat_core_crt = sat %>%
  filter(dist == "nucleo", parcela == "controle")

sat_core_b3y = sat %>%
  filter(dist == "nucleo", parcela == "b3yr")

sat_core_b1y = sat %>%
  filter(dist == "nucleo", parcela == "b1yr")

#Plot data
ggplot(sat_edge_crt, aes(date, value, color = variable))+
  geom_line(aes(group=variable), size = 1)+
  geom_point()
  











