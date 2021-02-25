####################################
# Time Series of Area-1 field data #
#                                  #
# Eduardo Q Marques 25-02-2021     #
####################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Data -----------------------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

lai = read.csv( "LAI_full_tang.csv", sep = ",")
litt = read.csv( "Liteira_full_tang.csv", sep = ",")
#biomass = read.csv( "xxxxxxx", sep = ",")

#Modify some elements -------------------------------------------------------------------
lai$parcela = as.character(lai$parcela)
lai$parcela[lai$parcela == "control"] <- c("Controle")
lai$parcela[lai$parcela == "b3yr"] <- c("B3yr")
lai$parcela[lai$parcela == "b1yr"] <- c("B1yr")

litt$parcela = as.character(litt$parcela)
litt$parcela[litt$parcela == "control"] <- c("Controle")
litt$parcela[litt$parcela == "b3yr"] <- c("B3yr")
litt$parcela[litt$parcela == "b1yr"] <- c("B1yr")

#Plot boxplots ---------------------------------------------------------------------------
eqm = c("orange", "red", "blue") #My color palette

#LAI
ggplot(lai, aes(x=year, y=lai, color = parcela))+
  geom_boxplot(aes(group = year))
















