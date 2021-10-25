####################################
# Time Series of Area-1 field data #
# (Hyper and Lansat) V2            #
# Eduardo Q Marques 22-10-2021     #
####################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Data -----------------------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

lai = read.csv( "LAI_full_tang.csv", sep = ",")
litt = read.csv( "Liteira_full_tang.csv", sep = ",")

#Modify some elements -------------------------------------------------------------------
#LAI
lai$parcela = as.character(lai$parcela)
lai$dist = as.character(lai$dist)
lai$parcela[lai$parcela == "control"] <- c("Controle")
lai$parcela[lai$parcela == "b3yr"] <- c("B3yr")
lai$parcela[lai$parcela == "b1yr"] <- c("B1yr")
lai$dist[lai$dist == "borda"] <- c("Borda")
lai$dist[lai$dist == "nucleo"] <- c("Interior")
lai = na.omit(lai)
lai$year = as.character(lai$year)
colnames(lai)[3:5] = c("LAI", "Ano", "Tratamento")

#Litterfall
litt$parcela = as.character(litt$parcela)
litt$dist = as.character(litt$dist)
litt$parcela[litt$parcela == "control"] <- c("Controle")
litt$parcela[litt$parcela == "b3yr"] <- c("B3yr")
litt$parcela[litt$parcela == "b1yr"] <- c("B1yr")
litt$dist[litt$dist == "borda"] <- c("Borda")
litt$dist[litt$dist == "nucleo"] <- c("Interior")
litt = na.omit(litt)
litt$year = as.character(litt$year)
colnames(litt)[1] = c("Tratamento")

#Vegetation Indices =======================================================================
hy = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')
land = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Modify and Filter Landsat data ===========================================================
land$year = substr(land$year, 1,4)
land$year = as.numeric(land$year)
land$index = as.character(land$index)
land$treat = as.character(land$treat)

land$index[land$index == "grnd"] <- c("GRND")
land$treat[land$treat == "control"] <- c("Controle")
land$treat[land$treat == "b3yr"] <- c("B3yr")
land$treat[land$treat == "b1yr"] <- c("B1yr")

#Resume repeat years with mean and select same years of Hyperion
land = land %>% 
  group_by(x, y, index, year, treat) %>% 
  summarise(value = mean(value)) %>% 
  filter(year >= 2004) %>% 
  filter(index == "GRND")

#Edge - Core separation
diffy = min(land$y) - max(land$y)
land$dist = ((max(land$y) - land$y)/diffy)*1000
land$dist = abs(land$dist)
summary(land$dist)

land$dist2 = c("a")
land$dist2[land$dist <= 250] = c("Borda")
land$dist2[land$dist > 250] = c("Interior")

colnames(land) = c("x","y","Indice","Ano","Tratamento","Valor","Dist","Dist2")

#Modify and Filter Hyperion data ==========================================================
hy$year = as.numeric(hy$year)
hy$index = as.character(hy$index)
hy$treat = as.character(hy$treat)

hy = hy %>% filter(index == "psri")
hy$index[hy$index == "psri"] <- c("PSRI")
hy$treat[hy$treat == "control"] <- c("Controle")
hy$treat[hy$treat == "b3yr"] <- c("B3yr")
hy$treat[hy$treat == "b1yr"] <- c("B1yr")

#Edge - Core separation
diffy = min(hy$y) - max(hy$y)
hy$dist = ((max(hy$y) - hy$y)/diffy)*1000
hy$dist = abs(hy$dist)
summary(hy$dist)

hy$dist2 = c("a")
hy$dist2[hy$dist <= 250] = c("Borda")
hy$dist2[hy$dist > 250] = c("Interior")

colnames(hy) = c("id1","id2","x","y","layer","Indice","Ano","Valor","Tratamento","Dist","Dist2")

#Join VIs and field data =================================================================
land = land %>% 
  select("Indice","Ano","Tratamento","Valor","Dist2")
land = land[,c(-1,-2)]

hy = hy %>% 
  na.omit() %>% 
  #filter(Valor < 0.3) %>% 
  select("Indice","Ano","Tratamento","Valor","Dist2")

lai$Indice = c("LAI")
lai$Ano = as.numeric(lai$Ano)
lai = lai %>% 
  select("Indice","Ano","Tratamento","LAI","dist")

litt$Indice = c("Liteira")
litt$year = as.numeric(litt$year)
litt = litt %>% 
  select("Indice","year","Tratamento","lit_ton_hec","dist") %>% 
  filter(year < 2020)

colnames(land) = c("Variavel","Ano","Tratamento","Valor","Dist")
colnames(hy) = c("Variavel","Ano","Tratamento","Valor","Dist")
colnames(lai) = c("Variavel","Ano","Tratamento","Valor","Dist")
colnames(litt) = c("Variavel","Ano","Tratamento","Valor","Dist")


df = full_join(land, hy)
df = full_join(df, lai)
df = full_join(df, litt)
df$Ano2 = as.character(df$Ano)

#Plot data =================================================================================
eqm = c("orange", "red", "blue") #My color palette
#Smooth time series
ggplot(df, aes(x=Ano2, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.3, size = 1)+
  geom_vline(xintercept = "2004", linetype = "dashed")+
  geom_vline(xintercept = "2011", linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  facet_grid(rows = vars(Variavel), cols = vars(Dist), scales = "free")+
  scale_color_manual(values = eqm)+
  #scale_fill_manual(values = eqm)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y = NULL, x = "Ano")+
  theme(text = element_text(family = "Times New Roman", size = 14))
  

#Smooth per Variable
a = df %>% 
  filter(Variavel == "LAI") %>% 
  ggplot(aes(x=Ano, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.3, size = 1)+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  facet_wrap(~Dist)+
  scale_color_manual(values = eqm)+
  xlim(2004, 2020)+
  theme_minimal()+
  labs(y = "LAI (m² m-²)", x = NULL)+
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.text.x = element_blank(), legend.position = "none")

b = df %>% 
  filter(Variavel == "Liteira") %>% 
  ggplot(aes(x=Ano, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.3, size = 1)+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  facet_wrap(~Dist)+
  scale_color_manual(values = eqm)+
  xlim(2004, 2020)+
  theme_minimal()+
  labs(y = "Mg Ano-¹", x = NULL)+
  theme(text = element_text(family = "Times New Roman", size = 14), legend.position = "none",
        axis.text.x = element_blank(), strip.text = element_blank())

c = df %>% 
  filter(Variavel == "GRND") %>% 
  ggplot(aes(x=Ano, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.3, size = 1)+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  facet_wrap(~Dist)+
  scale_color_manual(values = eqm)+
  xlim(2004, 2020)+
  theme_minimal()+
  labs(y = NULL, x = NULL)+
  theme(text = element_text(family = "Times New Roman", size = 14), legend.position = "none",
        axis.text.x = element_blank(), strip.text = element_blank())

d = df %>% 
  filter(Variavel == "PSRI") %>% 
  ggplot(aes(x=Ano, y=Valor, color = Tratamento))+
  #geom_smooth(aes(group=Tratamento), alpha = 0.3, size = 1, method = "gam")+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  stat_summary(geom="line", fun.data="mean_cl_boot",
               size = 1, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  facet_wrap(~Dist, drop = F)+
  scale_color_manual(values = eqm)+
  xlim(2004, 2020)+
  theme_minimal()+
  labs(y = NULL, x = "Ano")+
  theme(text = element_text(family = "Times New Roman", size = 14),
        strip.text = element_blank(), legend.position = "none")


#ggsave(filename = "LAI.png", plot = a,
 #   path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Dados de campo", width = 20, height = 7, units =  "cm", dpi = 300)

#ggsave(filename = "Liteira.png", plot = b,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Dados de campo", width = 20, height = 6.5, units =  "cm", dpi = 300)

#ggsave(filename = "GRND.png", plot = c,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Dados de campo", width = 20, height = 6.5, units =  "cm", dpi = 300)

#ggsave(filename = "PSRI.png", plot = d,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Dados de campo", width = 20, height = 7, units =  "cm", dpi = 300)

#Join plots
smt = ggarrange(a,b,c,d,
          common.legend = TRUE,
          legend="left",
          ncol = 1, nrow = 4,
          widths = 50)


ggsave(filename = "Smooth_Field and VIs.png", plot = smt,
     path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Dados de campo", width = 15, height = 15, units =  "cm", dpi = 300)
