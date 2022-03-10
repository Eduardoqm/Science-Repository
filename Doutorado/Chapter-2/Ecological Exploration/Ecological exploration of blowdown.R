######################################
# Ecological exploration of blowdown #
#                                    #
# Eduardo Q Marques 10-03-2022       #
######################################

library(tidyverse)
library(reshape2)

#First Part =================================================================================
#Merged field data
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento")

blowdown = read.csv("storm_field_data.csv", sep = ",")