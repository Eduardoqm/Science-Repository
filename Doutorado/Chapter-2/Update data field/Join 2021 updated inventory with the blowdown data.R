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

#Join inventory data -------------------------------------------------------------------------------
m5_10$placa = as.factor(m5_10$placa)
m40$placa = as.factor(m40$placa)
m40$inv = as.character(m40$inv)

#Full join
master = full_join(m5_10, m10_20)
master$obs_cic9 = as.factor(master$obs_cic9)

master = full_join(master, m40)

#Join Blowdown and inventory data ------------------------------------------------------------------







