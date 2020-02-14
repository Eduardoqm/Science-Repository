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
hyper = read.csv("Hyperion_indexs_median by plot.csv")

#hyper = hyper[,c(-12)] #PSSR have so high values in comparison with other indexs
hyper = hyper[,c(-9)] #NDWI have so high values in comparison with other indexs

#Separate by edge and core and tratament
#Landsat data
land_edge_crt = land %>%
  filter(dist == "borda", parcela == "controle")

land_edge_b3yr = land %>%
  filter(dist == "borda", parcela == "b3yr")

land_edge_b1yr = land %>%
  filter(dist == "borda", parcela == "b1yr")

land_core_crt = land %>%
  filter(dist == "nucleo", parcela == "controle")

land_core_b3yr = land %>%
  filter(dist == "nucleo", parcela == "b3yr")

land_core_b1yr = land %>%
  filter(dist == "nucleo", parcela == "b1yr")

#Hyperion data
hyper_edge_crt = hyper %>%
  filter(dist == "borda", parcela == "controle")

hyper_edge_b3yr = hyper %>%
  filter(dist == "borda", parcela == "b3yr")

hyper_edge_b1yr = hyper %>%
  filter(dist == "borda", parcela == "b1yr")

hyper_core_crt = hyper %>%
  filter(dist == "nucleo", parcela == "controle")

hyper_core_b3yr = hyper %>%
  filter(dist == "nucleo", parcela == "b3yr")

hyper_core_b1yr = hyper %>%
  filter(dist == "nucleo", parcela == "b1yr")


#Calculate difference ================
#Landsat Edge
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

#Landsat Core
land_core_crt$date = as.character(land_core_crt$date)
land_core_crt = melt(land_core_crt)

land_core_b3yr$date = as.character(land_core_b3yr$date)
land_core_b3yr = melt(land_core_b3yr)

land_core_b1yr$date = as.character(land_core_b1yr$date)
land_core_b1yr = melt(land_core_b1yr)


land_diff_core = land_core_crt[,c(1,2,3,4)]
land_diff_core$b3yr = ((land_core_b3yr$value-land_core_crt$value)*100)/land_core_crt$value
land_diff_core$b1yr = ((land_core_b1yr$value-land_core_crt$value)*100)/land_core_crt$value

land_diff_core = land_diff_core[,c(-1)]


#Hyperion Edge
hyper_edge_crt$date = as.character(hyper_edge_crt$date)
hyper_edge_crt = melt(hyper_edge_crt)

hyper_edge_b3yr$date = as.character(hyper_edge_b3yr$date)
hyper_edge_b3yr = melt(hyper_edge_b3yr)

hyper_edge_b1yr$date = as.character(hyper_edge_b1yr$date)
hyper_edge_b1yr = melt(hyper_edge_b1yr)


hyper_diff_edge = hyper_edge_crt[,c(1,2,3,4)]
hyper_diff_edge$b3yr = ((hyper_edge_b3yr$value-hyper_edge_crt$value)*100)/hyper_edge_crt$value
hyper_diff_edge$b1yr = ((hyper_edge_b1yr$value-hyper_edge_crt$value)*100)/hyper_edge_crt$value

hyper_diff_edge = hyper_diff_edge[,c(-1)]

#Hyperion Core
hyper_core_crt$date = as.character(hyper_core_crt$date)
hyper_core_crt = melt(hyper_core_crt)

hyper_core_b3yr$date = as.character(hyper_core_b3yr$date)
hyper_core_b3yr = melt(hyper_core_b3yr)

hyper_core_b1yr$date = as.character(hyper_core_b1yr$date)
hyper_core_b1yr = melt(hyper_core_b1yr)


hyper_diff_core = hyper_core_crt[,c(1,2,3,4)]
hyper_diff_core$b3yr = ((hyper_core_b3yr$value-hyper_core_crt$value)*100)/hyper_core_crt$value
hyper_diff_core$b1yr = ((hyper_core_b1yr$value-hyper_core_crt$value)*100)/hyper_core_crt$value

hyper_diff_core = hyper_diff_core[,c(-1)]


#Landsat Plots ========================================================================
#Landsat edge ================
gg_edge = melt(land_diff_edge)
colnames(gg_edge) = c("date","dist", "index", "parcela", "value")
gg1 = gg_edge %>% filter(parcela == 'b1yr')
gg2 = gg_edge %>% filter(parcela == 'b3yr')

ggplot(gg1, aes(date,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  #geom_point()+
  labs(fill= "Plot",x="Year",y="B1yr - Control (% Relative difference Edge)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 10, label = "Dry")+

  
  annotate("text", x = 2.5, y = -123, label = "Before fire", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -129, ymax = -135, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -123, label = "Fire experiment", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -129, ymax = -135, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -130, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -130, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -123, label = "Restoration", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -129, ymax = -135, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


ggplot(gg2, aes(date,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  #geom_point()+
  labs(fill= "Plot",x="Year",y="B3yr - Control (% Relative difference Edge)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 10, label = "Dry")+
  
  
  annotate("text", x = 2.5, y = -222, label = "Before fire", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -229, ymax = -236, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -222, label = "Fire experiment", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -229, ymax = -236, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -230, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -230, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -222, label = "Restoration", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -229, ymax = -236, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


#Landsat core ================
gg_core = melt(land_diff_core)
colnames(gg_core) = c("date","dist", "index", "parcela", "value")
gg3 = gg_core %>% filter(parcela == 'b1yr')
gg4 = gg_core %>% filter(parcela == 'b3yr')

ggplot(gg3, aes(date,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  #geom_point()+
  labs(fill= "Plot",x="Year",y="B1yr - Control (% Relative difference Core)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 10, label = "Dry")+
  
  
  annotate("text", x = 2.5, y = -75, label = "Before fire", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -83, ymax = -89, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -75, label = "Fire experiment", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -83, ymax = -89, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -85, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -85, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -75, label = "Restoration", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -83, ymax = -89, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


ggplot(gg4, aes(date,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  #geom_point()+
  labs(fill= "Plot",x="Year",y="B3yr - Control (% Relative difference Core)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 10, label = "Dry")+
  
  
  annotate("text", x = 2.5, y = -70, label = "Before fire", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -75, ymax = -80, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -70, label = "Fire experiment", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -75, ymax = -80, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -76, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -76, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -70, label = "Restoration", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -75, ymax = -80, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


#Hyperion Plots ==========================================================================
#Hyperion edge ================
gg_edge = melt(hyper_diff_edge)
colnames(gg_edge) = c("date","dist", "index", "parcela", "value")
gg1 = gg_edge %>% filter(parcela == 'b1yr')
gg2 = gg_edge %>% filter(parcela == 'b3yr')

ggplot(gg1, aes(date,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  #geom_point()+
  labs(fill= "Plot",x="Year",y="B1yr - Control (% Relative difference Edge)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(3.5, 5.5), y = 110, label = "Dry")+
  
  
  annotate("text", x = 3, y = -123, label = "Fire experiment", colour = "red")+
  annotate("rect", xmin = 1, xmax = 6, ymin = -129, ymax = -135, alpha = 0.9, fill = "red")+
  annotate("text", x = c(1.5, 3.7, 5.5), y = -130, label = "xx", colour = "yellow")+
  
  annotate("text", x = 6.5, y = -123, label = "Restoration", colour = "darkblue")+
  annotate("rect", xmin = 6, xmax = 7, ymin = -129, ymax = -135, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


ggplot(gg2, aes(date,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  #geom_point()+
  labs(fill= "Plot",x="Year",y="B3yr - Control (% Relative difference Edge)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(3.5, 5.5), y = 110, label = "Dry")+
  
  
  annotate("text", x = 3, y = -123, label = "Fire experiment", colour = "red")+
  annotate("rect", xmin = 1, xmax = 6, ymin = -129, ymax = -135, alpha = 0.9, fill = "red")+
  annotate("text", x = c(1.5, 3.7, 5.5), y = -130, label = "xx", colour = "yellow")+
  
  annotate("text", x = 6.5, y = -123, label = "Restoration", colour = "darkblue")+
  annotate("rect", xmin = 6, xmax = 7, ymin = -129, ymax = -135, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


#Hyperion core ================
gg_core = melt(hyper_diff_core)
colnames(gg_core) = c("date","dist", "index", "parcela", "value")
gg3 = gg_core %>% filter(parcela == 'b1yr')
gg4 = gg_core %>% filter(parcela == 'b3yr')

ggplot(gg3, aes(date,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  #geom_point()+
  labs(fill= "Plot",x="Year",y="B1yr - Control (% Relative difference Core)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(3.5, 5.5), y = 110, label = "Dry")+
  
  
  annotate("text", x = 3, y = -123, label = "Fire experiment", colour = "red")+
  annotate("rect", xmin = 1, xmax = 6, ymin = -129, ymax = -135, alpha = 0.9, fill = "red")+
  annotate("text", x = c(1.5, 3.7, 5.5), y = -130, label = "xx", colour = "yellow")+
  
  annotate("text", x = 6.5, y = -123, label = "Restoration", colour = "darkblue")+
  annotate("rect", xmin = 6, xmax = 7, ymin = -129, ymax = -135, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(gg4, aes(date,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  #geom_point()+
  labs(fill= "Plot",x="Year",y="B3yr - Control (% Relative difference Core)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(3.5, 5.5), y = 110, label = "Dry")+
  
  
  annotate("text", x = 3, y = -123, label = "Fire experiment", colour = "red")+
  annotate("rect", xmin = 1, xmax = 6, ymin = -129, ymax = -135, alpha = 0.9, fill = "red")+
  annotate("text", x = c(1.5, 3.7, 5.5), y = -130, label = "xx", colour = "yellow")+
  
  annotate("text", x = 6.5, y = -123, label = "Restoration", colour = "darkblue")+
  annotate("rect", xmin = 6, xmax = 7, ymin = -129, ymax = -135, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))
