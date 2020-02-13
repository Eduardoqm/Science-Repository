#Organize biomass by Edge and Core on Area-1

#Eduardo Q Marques 13/02/2020

library(tidyverse)
library(reshape2)

#Data Bank ==================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Area1-plot/Fogo")

biomass = read.csv("Biomassa_Growth_Tanguro_Brando_Recovery_GCB_version5.csv", sep = ",", header = TRUE)

#Part 1 - organize data =====================================================================
biomass = biomass %>%
  select(parcela, transecto, dbh.class, dap_Bio, dap_c08_Bio, dap_c10_Bio, dap_c11_Bio, dap_c12_Bio, dap.14_Bio, dap.16_Bio)
colnames(biomass) = c('plot', 'transcto', 'dbh.class', '2004', '2008', '2010', '2011', '2012', '2014', '2016')

biomass = melt(biomass)
colnames(biomass) = c('plot', 'transcto', 'dbh.class', 'data', 'biomass')

biomass$transcto = as.character(biomass$transcto)
biomass$plot = as.character(biomass$plot)
biomass$dbh.class = as.character(biomass$dbh.class)

#Add plot info
biomass$plot[biomass$plot == "A"] <- c("controle")
biomass$plot[biomass$plot == "B"] <- c("b3yr")
biomass$plot[biomass$plot == "C"] <- c("b1yr")
