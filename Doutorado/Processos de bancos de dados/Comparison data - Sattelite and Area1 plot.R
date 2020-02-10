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

#hyper = hyper[,c(-12)] #PSSR have so high values in comparison with other indexs

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

  
ggplot(sat_edge_b3y, aes(date, value, color = variable))+
  geom_line(aes(group=variable), size = 1)

ggplot(sat_edge_b1y, aes(date, value, color = variable))+
  geom_line(aes(group=variable), size = 1)+
  geom_point()


#Calculate difference
diff_edge = sat_edge_crt[,c(1,2,3,4)]
diff_edge$b3yr = ((sat_edge_b3y$value-sat_edge_crt$value)*100)/sat_edge_crt$value
diff_edge$b1yr = ((sat_edge_b1y$value-sat_edge_crt$value)*100)/sat_edge_crt$value


diff_core = sat_core_crt[,c(1,2,3,4)]
diff_core$b3yr = ((sat_core_b3y$value-sat_core_crt$value)*100)/sat_core_crt$value
diff_core$b1yr = ((sat_core_b1y$value-sat_core_crt$value)*100)/sat_core_crt$value




gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no WBI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()










