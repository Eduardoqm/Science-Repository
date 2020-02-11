#Plots to comparation Insdex data and Area1 data

#Eduardo Q Marques 06-02-2020
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)
library(plotly)

#Data bank
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1')

#Lansat
land = read.csv("Landast_indexs_median by plot.csv")

#landsat = landsat[,c(-6)] #NDWI have so high values in comparison with other indexs

#Hyperion
hyper = read.csv("Hyperion_indexs_median by plot_2.csv")

#hyper = hyper[,c(-12)] #PSSR have so high values in comparison with other indexs
#hyper = hyper[,c(-9)] #NDWI have so high values in comparison with other indexs

#Process to plot ==================
#Calculate difference
diff_edge = sat_edge_crt[,c(1,2,3,4)]

diff_edge$b3yr = ((sat_edge_b3y$value-sat_edge_crt$value)*100)/sat_edge_crt$value
diff_edge$b1yr = ((sat_edge_b1y$value-sat_edge_crt$value)*100)/sat_edge_crt$value

diff_edge = diff_edge[,c(-1)]


diff_core = sat_core_crt[,c(1,2,3,4)]

diff_core$b3yr = ((sat_core_b3y$value-sat_core_crt$value)*100)/sat_core_crt$value
diff_core$b1yr = ((sat_core_b1y$value-sat_core_crt$value)*100)/sat_core_crt$value

diff_core = diff_core[,c(-1)]

#Separate by edge and core
land_edge = land %>%
  filter(dist == "borda")

land_core = land %>%
  filter(dist == "nucleo")

hyper_edge = hyper %>%
  filter(dist == "borda")

hyper_core = hyper %>%
  filter(dist == "nucleo")















#Plot data ===============
gg = melt(diff_edge)
colnames(gg) = c("date","dist", "index", "parcela", "value")

ggplot(gg, aes(date,value, col=index))+ 
  #geom_line(aes(group=index), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()




p <- ggplot(gg, aes(date,value, col=index))+ 
  #geom_line(aes(group=index), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

#ggplotly(p)