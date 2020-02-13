#Organize biomass by Edge and Core on Area-1

#Eduardo Q Marques 13/02/2020

library(tidyverse)
library(reshape2)

#Data Bank
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Area1-plot/Fogo")

biomass = read.csv("Biomassa_Growth_Tanguro_Brando_Recovery_GCB_version5.csv", sep = ",", header = TRUE)
