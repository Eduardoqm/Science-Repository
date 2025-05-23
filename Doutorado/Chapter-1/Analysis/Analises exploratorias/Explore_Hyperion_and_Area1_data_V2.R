#Analysis Cap-1 Hyperion Indexs on Fire
#By: Eduardo Q Marques 14-02-2020
#Obs: looking Analysis Cap1.R

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggpubr)
library(plotly)

#Load data ==========================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

hy = read.csv("Hyperion_indexs_median by plot.csv", sep = ",", header = TRUE)
hy = hy[,c(-9)] #NDWI have so high values in comparison with other indexs

#hy = read.csv("Hyperion_indexs_all by plot.csv", sep = ',')
#hy$data = as.character(hy$data)

biomass = read.csv("Biomass_tang.csv", sep = ",", header = TRUE)

lai = read.csv("LAI_tang.csv", sep = ",", header = TRUE)

litt  = read.csv("Liteira_tang.csv", sep = ",", header = TRUE)

fuel = read.csv("Fuel_tang.csv", sep = ",", header = TRUE)

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

fuel = fuel %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')
colnames(fuel) = c("id", "fuel")


df = hy
#df = full_join(df, biomass, by="id")
df = full_join(df, lai, by="id")
df = full_join(df, litt, by="id")
#df = full_join(df, fuel, by="id")

df = df %>% 
  separate(col = "id", c("parcela", "data", "dist"), sep = '_')


#write.table(df, "Integration_Area1_data_edge_core.csv", sep = ",")

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

#df_diff_edge$b3yr = abs(df_diff_edge$b3yr)#Convert to positive
#df_diff_edge$b1yr = abs(df_diff_edge$b1yr)

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

#df_diff_core$b3yr = abs(df_diff_core$b3yr)#Convert to positive
#df_diff_core$b1yr = abs(df_diff_core$b1yr)

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

#a; b; c; d

#Join plots by the groups ===============================================================
#Struture Edge ========
struc_edge = df_diff_edge %>% 
  filter(variable %in% c('evi','ndvi','nbri','vari','vig','biomass','lai','litter','fuel'))

struc_edge = melt(struc_edge)
colnames(struc_edge) = c("data","dist", "index", "parcela", "value")

struc_edge_b1yr = struc_edge %>% filter(parcela == 'b1yr')
struc_edge_b3yr = struc_edge %>% filter(parcela == 'b3yr')


a = ggplot(struc_edge_b1yr, aes(data,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  labs(fill= "Index",x="Ano",y="B1yr - Controle (% Relative difference)",
       title = "Structural Indexes (Edge)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))


b = ggplot(struc_edge_b3yr, aes(data,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

#Struture Core
struc_core = df_diff_core %>% 
  filter(variable %in% c('evi','ndvi','nbri','vari','vig','biomass','lai','litter','fuel'))

struc_core = melt(struc_core)
colnames(struc_core) = c("data","dist", "index", "parcela", "value")

struc_core_b1yr = struc_core %>% filter(parcela == 'b1yr')
struc_core_b3yr = struc_core %>% filter(parcela == 'b3yr')


c = ggplot(struc_core_b1yr, aes(data,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  labs(fill= "Index",x="Ano",y="B1yr - Controle (% Relative difference)",
       title = "Structural Indexes (Core)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))


d = ggplot(struc_core_b3yr, aes(data,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

struc = ggarrange(a + rremove("xlab"),
                  c + rremove("xylab"),
                  b, d + rremove("ylab"),
                  common.legend = TRUE,
                  legend="bottom",
                  ncol = 2, nrow = 2)

#Biochemistry Edge ========
bioc_edge = df_diff_edge %>% 
  filter(variable %in% c('ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','biomass','lai','litter','fuel'))

bioc_edge = melt(bioc_edge)
colnames(bioc_edge) = c("data","dist", "index", "parcela", "value")

bioc_edge_b1yr = bioc_edge %>% filter(parcela == 'b1yr')
bioc_edge_b3yr = bioc_edge %>% filter(parcela == 'b3yr')


a = ggplot(bioc_edge_b1yr, aes(data,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  labs(fill= "Index",x="Ano",y="B1yr - Controle (% Relative difference)",
       title = "Biochemistry Indexes (Edge)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))


b = ggplot(bioc_edge_b3yr, aes(data,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

#Biochemistry Core
bioc_core = df_diff_core %>% 
  filter(variable %in% c('ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','biomass','lai','litter','fuel'))

bioc_core = melt(bioc_core)
colnames(bioc_core) = c("data","dist", "index", "parcela", "value")

bioc_core_b1yr = bioc_core %>% filter(parcela == 'b1yr')
bioc_core_b3yr = bioc_core %>% filter(parcela == 'b3yr')


c = ggplot(bioc_core_b1yr, aes(data,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  labs(fill= "Index",x="Ano",y="B1yr - Controle (% Relative difference)",
       title = "Biochemistry Indexes (Core)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))


d = ggplot(bioc_core_b3yr, aes(data,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

bioc = ggarrange(a + rremove("xlab"),
                 c + rremove("xylab"),
                 b, d + rremove("ylab"),
                 common.legend = TRUE,
                 legend="bottom",
                 ncol = 2, nrow = 2)

#Physiologic Edge ========
phy_edge = df_diff_edge %>% 
  filter(variable %in% c('pri','rendvi','biomass','lai','litter','fuel'))

phy_edge = melt(phy_edge)
colnames(phy_edge) = c("data","dist", "index", "parcela", "value")

phy_edge_b1yr = phy_edge %>% filter(parcela == 'b1yr')
phy_edge_b3yr = phy_edge %>% filter(parcela == 'b3yr')


a = ggplot(phy_edge_b1yr, aes(data,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  labs(fill= "Index",x="Ano",y="B1yr - Controle (% Relative difference)",
       title = "Physiologic Indexes (Edge)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))


b = ggplot(phy_edge_b3yr, aes(data,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

#Physiologic Core
phy_core = df_diff_core %>% 
  filter(variable %in% c('pri','rendvi','biomass','lai','litter','fuel'))

phy_core = melt(phy_core)
colnames(phy_core) = c("data","dist", "index", "parcela", "value")

phy_core_b1yr = phy_core %>% filter(parcela == 'b1yr')
phy_core_b3yr = phy_core %>% filter(parcela == 'b3yr')


c = ggplot(phy_core_b1yr, aes(data,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  labs(fill= "Index",x="Ano",y="B1yr - Controle (% Relative difference)",
       title = "Physiologic Indexes (Core)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))


d = ggplot(phy_core_b3yr, aes(data,value, col=index))+ 
  geom_line(aes(group=index), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

phy = ggarrange(a + rremove("xlab"),
                c + rremove("xylab"),
                b, d + rremove("ylab"),
                common.legend = TRUE,
                legend="bottom",
                ncol = 2, nrow = 2)
