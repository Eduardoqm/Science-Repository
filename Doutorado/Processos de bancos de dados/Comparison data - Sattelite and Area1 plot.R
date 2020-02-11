#Plots to comparation Insdex data and Area1 data

#Eduardo Q Marques 06-02-2020
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)
library(plotly)

#Data bank =======================
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1')

#Lansat
land = read.csv("Landast_indexs_median by plot.csv")

#landsat = landsat[,c(-6)] #NDWI have so high values in comparison with other indexs

#Hyperion
hyper = read.csv("Hyperion_indexs_median by plot_2.csv")

#hyper = hyper[,c(-12)] #PSSR have so high values in comparison with other indexs
#hyper = hyper[,c(-9)] #NDWI have so high values in comparison with other indexs

#Process to plot ==================
#Separate by edge and core and tratament
#Landsat data
land_edge_crt = land %>%
  filter(dist == "borda", parcela == "controle")

land_edge_b3yr = land %>%
  filter(dist == "borda", parcela == "b3yr")

land_edge_b1yr = land %>%
  filter(dist == "borda", parcela == "b1yr")

land_core_crt = land %>%
  filter(dist == "borda", parcela == "controle")

land_core_b3yr = land %>%
  filter(dist == "borda", parcela == "b3yr")

land_core_b1yr = land %>%
  filter(dist == "borda", parcela == "b1yr")

#Hyperion data
hyper_edge_crt = hyper %>%
  filter(dist == "borda", parcela == "controle")

hyper_edge_b3yr = hyper %>%
  filter(dist == "borda", parcela == "b3yr")

hyper_edge_b1yr = hyper %>%
  filter(dist == "borda", parcela == "b1yr")

hyper_core_crt = hyper %>%
  filter(dist == "borda", parcela == "controle")

hyper_core_b3yr = hyper %>%
  filter(dist == "borda", parcela == "b3yr")

hyper_core_b1yr = hyper %>%
  filter(dist == "borda", parcela == "b1yr")


#Calculate difference ================
land_edge_crt$date = as.character(land_edge_crt$date)
land_edge_crt = melt(land_edge_crt)

land_edge_b3yr$date = as.character(land_edge_b3yr$date)
land_edge_b3yr = melt(land_edge_b3yr)

land_edge_b1yr$date = as.character(land_edge_b1yr$date)
land_edge_b1yr = melt(land_edge_b1yr)


land_diff_edge = land_edge_crt[,c(1,2,3,4)]
land_diff_edge$b3yr = ((land_edge_b3yr$value-land_edge_crt$value)*100)/land_edge_crt$value
land_diff_edge$b1yr = ((land_edge_b1yr$value-land_edge_crt$value)*100)/land_edge_crt$value

land_diff_edge = land_diff_edge[,c(-1)]


diff_core = sat_core_crt[,c(1,2,3,4)]

diff_core$b3yr = ((sat_core_b3y$value-sat_core_crt$value)*100)/sat_core_crt$value
diff_core$b1yr = ((sat_core_b1y$value-sat_core_crt$value)*100)/sat_core_crt$value

diff_core = diff_core[,c(-1)]






land_edge = land_edge[,c(-9)]
land_edge$date = as.character(land_edge$date)
land_edge = melt(land_edge)
colnames(land_edge) = c("parcela", "date", "index", "value")






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