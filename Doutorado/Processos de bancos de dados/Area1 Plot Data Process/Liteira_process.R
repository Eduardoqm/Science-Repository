#Organize Litterfall by Edge and Core on Area-1

#Eduardo Q Marques 13/02/2020

library(tidyverse)
library(reshape2)

#Data Bank ==================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Area1-plot/Tanguro Parcela")

litt = read.csv("1_master_liteira_area_1_jun2019.csv", sep = ",", header = TRUE)

#Part 1 - Organize data =====================================================================


#Part 2 - Calculate tons by hectare==========================================================
#We know that it has 500 meters per transect and now we have the width of the transect. We will have to multiply to have the area in m2 and transform it into hectares.




#Part 3 - Export data as CSV ================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

#write.table(litt, "Liteira_tang.csv", sep = ",")












