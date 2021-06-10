######################################################
# Join 2021 updated inventory with the blowdown data #
#                                                    #
# Eduardo Q Marques 01-04-2021                       #
######################################################

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

#Filter Blowdown trees only and data of interest ---------------------------------------------------
df2 = df %>% 
  filter(parcela_bl != "NA") #I am surprised...it's work!

#df3 = df2[,c(1,13,16,14,15,17,3,4,5,6,7,8,9,10,11,12,19,20,22,23,24,25,26,27,30,72)]
df3 = df[,c(1,13,16,14,15,17,3,4,5,6,7,8,9,10,11,12,19,20,22,23,24,25,26,27,30,72,128,129)]

#Join with Sucessional data ------------------------------------------------------------------------
df4 = full_join(df3, suce)
df5 = df4 %>% 
  filter(placa != "NA")
#df5 = df5[,c(-24)]

#Inform some species that no match with the list
df5$succ[df5$codigo == "Tacvul"] <- c("PIO")
df5$succ[df5$codigo == "Inghet"] <- c("CLI")

#Inform some species that no match with the species names
df5$species = as.character(df5$species)
df5$codigo = as.character(df5$codigo)
df5$species[df5$codigo == "Inghet"] <- c("Inga_heterophylla")
df5$species[df5$codigo == "Tacvul"] <- c("Tachigali_vulgaris")

df6 = df5 %>% 
  filter(parcela_bl != "NA") #I am surprised...it's work!

#Export data ---------------------------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento")

write.table(df6, file = "blowdown_full_update_2021_B.csv", sep = ",", row.names = F)





