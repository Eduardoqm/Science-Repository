#Process to using Silverio's Script: Figure.generaxdamaged by plots

#Eduardo Q Marques 02-09-2021

#First Part ========================================================================================
#Join 2021 updated inventory with the blowdown data - EQM 06-07-2021
library(tidyverse)
library(reshape2)

#Open data -----------------------------------------------------------------------------------------
#Blowdown field data
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento")

blowdown = read.csv("storm_field_data.csv", sep = ",")

#Inventory data
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Invetarios florestais")

m5_10 = read.csv("master510_area1_Novembro_2020.csv", sep = ",")
m10_20 = read.csv("master1020_area1_Novembro_2020.csv", sep = ",")
m40 = read.csv("master40_area1_Novembro_2020.csv", sep = ",")

#Sucessional group data
suce = read.csv("grupo-sucessional-tanguro.csv", sep = ",")
colnames(suce)[1] = c("codigo")

#Join inventory data -------------------------------------------------------------------------------
m5_10$placa = as.factor(m5_10$placa)
m40$placa = as.factor(m40$placa)
m40$inv = as.character(m40$inv)

#Full join
master = full_join(m5_10, m10_20)
master$obs_cic9 = as.factor(master$obs_cic9)

master = full_join(master, m40)

#Join Blowdown and inventory data ------------------------------------------------------------------
blowdown$placa = as.character(blowdown$placa)
df = full_join(blowdown, master, by = "placa")

df2 = df %>% 
  filter(is.na(data_morta))

#Filter Blowdown trees only and data of interest ---------------------------------------------------
df3 = df2[,c(1,13,16,14,15,17,3,4,5,6,7,8,9,10,11,12,19,20,22,23,24,25,26,27,30,72,127,128,129)]

#Join with Sucessional data ------------------------------------------------------------------------
df4 = full_join(df3, suce)
df5 = df4 %>% 
  filter(placa != "NA")

#Inform some species that no match with the list
df5$succ[df5$codigo == "Tacvul"] <- c("PIO")
df5$succ[df5$codigo == "Inghet"] <- c("CLI")

#Inform some species that no match with the species names
df5$species = as.character(df5$species)
df5$codigo = as.character(df5$codigo)
df5$species[df5$codigo == "Inghet"] <- c("Inga_heterophylla")
df5$species[df5$codigo == "Tacvul"] <- c("Tachigali_vulgaris")

#Calc percentege
df5$cont = 1
df5$tipo_de_dano = as.character(df5$tipo_de_dano)
df5$tipo_de_dano <- replace(df5$tipo_de_dano,is.na(df5$tipo_de_dano), "Intact")

df6 = df5 %>% 
  #na.omit() %>% 
  group_by(genero, tipo_de_dano) %>% 
  summarise(cont = sum(cont))

maxtree = df5 %>%  
  group_by(genero) %>% 
  summarise(maxcont = sum(cont))

df5b = df %>% 
  select(genero, densidade)

mdens = df5b %>% 
  na.omit() %>%
  group_by(genero) %>%
  summarise(densidade = mean(densidade))

df7 = full_join(df6, maxtree, by = "genero")
df7 = full_join(df7, mdens, by = "genero")

df7$prc = ((df7$cont*100)/df7$maxcont)

df8 = df7 %>%
  na.omit() %>% 
  filter(tipo_de_dano != "Intact") 
df8$tipo_de_dano[df8$tipo_de_dano == "Broken"] <- c("Snapped")
df8$tipo_de_dano[df8$tipo_de_dano == "Crown"] <- c("Crown-damage")

abund = df8 %>% 
  group_by(genero) %>% 
  summarise(abund = sum(cont))

frec = df8 %>% 
  group_by(genero) %>% 
  summarise(frec = sum(prc))

df8 = full_join(df8, abund, by = "genero")
df8 = full_join(df8, frec, by = "genero")
df8$prc2 = df8$prc/100
df8$densidade = format(round(df8$densidade, 2), nsmall = 2)

df8 = df8 %>% 
  filter(abund >= 5)

library(ggplot2)
library(ggthemes)
library(doBy)

fs4 = ggplot(df8, aes(x = reorder(genero, -frec), y = prc2)) +
  coord_flip()+
  geom_bar(aes(fill = factor(tipo_de_dano, levels = c("Crown-damage", "Uprooted", "Snapped"))),stat = 'identity') +
  ylim(c(-0.0035, 0.125))+
  xlab("") +
  ylab("Damage frequency") +
  geom_text(aes(x = reorder(genero, frec), y = -0.0035, label = maxcont),size=3)+
  geom_text(aes(x = reorder(genero, frec), y = (frec/100) + 0.001,
               label = densidade),size=3,hjust = 0)+
  theme_grey(base_size = 13)+
  theme_bw()+
  scale_fill_manual(breaks = c("Crown-damage", "Uprooted", "Snapped"),
                    values = c("#000000", "#E79F01", "#53B4EB"))+
  theme(legend.title=element_blank(),
        axis.title.x=element_text(size=14),
        axis.text.y = element_text(face = "italic"),
        legend.position=c(.76,.66),
        legend.key.width = unit(1.2, "cm"))

fs4

#ggsave(filename = "Figure.generoxdamage_eqm.png", plot = fs4,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Clone plots of Silverio", 
 #      width = 13, height = 14, units =  "cm", dpi = 300)



