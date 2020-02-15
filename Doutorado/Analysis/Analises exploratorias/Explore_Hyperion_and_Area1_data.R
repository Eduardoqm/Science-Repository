#Analysis Cap-1 Hyperion Indexs on Fire
#By: Eduardo Q Marques 14-02-2020
#Obs: looking Analysis Cap1.R

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(plotly)

#Load data ==========================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

hy = read.csv("Hyperion_indexs_median by plot.csv", sep = ",", header = TRUE)
hy = hy[,c(-9)] #NDWI have so high values in comparison with other indexs

biomass = read.csv("Biomass_tang.csv", sep = ",", header = TRUE)

lai = read.csv("LAI_tang.csv", sep = ",", header = TRUE)

litt  = read.csv("Liteira_tang.csv", sep = ",", header = TRUE)

#fuel = read.csv("Combustivel_Brown_Tang.csv", sep = ",", header = TRUE)
#fire = read.csv("Fire.csv", sep = ",", header = TRUE) #Make the fire intensity!


#Data integration =======================================================================
#From Hyperion we have 2004:2012, so let's try use other datas in this range
#Biomass
biomass = biomass %>% 
  filter(data <=2012)

#LAI
lai = lai %>% 
  filter(data <=2012)

#Litterfall
litt = litt %>% 
  filter(data <=2012)

#Join Data
hy = hy %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

biomass = as.data.frame(biomass)#To work
biomass = biomass %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')
colnames(biomass) = c("id", "biomass")

lai = lai %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

litt = litt %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')
colnames(litt) = c("id", "litter")



df = hy
df = full_join(df, biomass, by="id")
df = full_join(df, lai, by="id")
df = full_join(df, litt, by="id")

df = df %>% 
  separate(col = "id", c("parcela", "data", "dist"), sep = '_')


#write.table(df, "Integration_Area1_data_edge_core.csv", sep = ",")
#Correlation Matrix ======================================================================
#ggpairs(df)

#Data organization =======================================================================
#Separate by edge and core and tratament
df_edge_crt = df %>%
  filter(dist == "borda", parcela == "controle")

df_edge_b3yr = df %>%
  filter(dist == "borda", parcela == "b3yr")

df_edge_b1yr = df %>%
  filter(dist == "borda", parcela == "b1yr")

df_core_crt = df %>%
  filter(dist == "nucleo", parcela == "controle")

df_core_b3yr = df %>%
  filter(dist == "nucleo", parcela == "b3yr")

df_core_b1yr = df %>%
  filter(dist == "nucleo", parcela == "b1yr")

#Calculate difference in relation to control
#df Edge
df_edge_crt$data = as.character(df_edge_crt$data)
df_edge_crt = melt(df_edge_crt)

df_edge_b3yr$data = as.character(df_edge_b3yr$data)
df_edge_b3yr = melt(df_edge_b3yr)

df_edge_b1yr$data = as.character(df_edge_b1yr$data)
df_edge_b1yr = melt(df_edge_b1yr)


df_diff_edge = df_edge_crt[,c(1,2,3,4)]
df_diff_edge$b3yr = ((df_edge_b3yr$value-df_edge_crt$value)*100)/df_edge_crt$value
df_diff_edge$b1yr = ((df_edge_b1yr$value-df_edge_crt$value)*100)/df_edge_crt$value

df_diff_edge = df_diff_edge[,c(-1)]

df_diff_edge = df_diff_edge %>% 
  na.omit()

#df Core
df_core_crt$data = as.character(df_core_crt$data)
df_core_crt = melt(df_core_crt)

df_core_b3yr$data = as.character(df_core_b3yr$data)
df_core_b3yr = melt(df_core_b3yr)

df_core_b1yr$data = as.character(df_core_b1yr$data)
df_core_b1yr = melt(df_core_b1yr)


df_diff_core = df_core_crt[,c(1,2,3,4)]
df_diff_core$b3yr = ((df_core_b3yr$value-df_core_crt$value)*100)/df_core_crt$value
df_diff_core$b1yr = ((df_core_b1yr$value-df_core_crt$value)*100)/df_core_crt$value

df_diff_core = df_diff_core[,c(-1)]

df_diff_core = df_diff_core %>% 
  na.omit()

#DF Plots ==========================================================================
#DF edge ================
gg_edge = melt(df_diff_edge)
colnames(gg_edge) = c("date","dist", "index", "parcela", "value")
gg1 = gg_edge %>% filter(parcela == 'b1yr')
gg2 = gg_edge %>% filter(parcela == 'b3yr')

a = ggplot(gg1, aes(date,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Year",y="B1yr - Control (% Relative difference Edge)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(3.5, 5.5), y = 110, label = "Dry")+
  
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


b = ggplot(gg2, aes(date,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Year",y="B3yr - Control (% Relative difference Edge)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(3.5, 5.5), y = 110, label = "Dry")+
  
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


#DF core ================
gg_core = melt(df_diff_core)
colnames(gg_core) = c("date","dist", "index", "parcela", "value")
gg3 = gg_core %>% filter(parcela == 'b1yr')
gg4 = gg_core %>% filter(parcela == 'b3yr')

c = ggplot(gg3, aes(date,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Year",y="B1yr - Control (% Relative difference Core)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(3.5, 5.5), y = 50, label = "Dry")+
  
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

d = ggplot(gg4, aes(date,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Year",y="B3yr - Control (% Relative difference Core)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(3.5, 5.5), y = 50, label = "Dry")+
  
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

a; b; c; d
#Join plots ===================================================================
#j = ggarrange(a, c, ncol = 2, nrow = 1)

#b = ggarrange(b, d, ncol = 2, nrow = 1)

#ggarrange(a, b, ncol = 1, nrow = 2)

#c = ggarrange(a, b, c, d + font("x.text", size = 10),
          #ncol = 2, nrow = 2)

#a = ggplotly(a)
#b = ggplotly(b)
#c = ggplotly(c)
#d = ggplotly(d)

