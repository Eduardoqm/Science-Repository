#Organize biomass on Area-1 (Total of biomass)

#Eduardo Q Marques 30-03-2021

library(tidyverse)
library(reshape2)

#Data Bank ==================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Fogo")

biomass = read.csv("Biomassa_Growth_Tanguro_Brando_Recovery_GCB_version5.csv", sep = ",", header = TRUE)

#Part 1 - Organize data =====================================================================
biomass = biomass %>%
  select(parcela, transecto, dbh.class, dap_Bio, dap_c08_Bio, dap_c10_Bio, dap_c11_Bio, dap_c12_Bio, dap.14_Bio, dap.16_Bio)
colnames(biomass) = c('plot', 'transcto', 'dbh.class', '2004', '2008', '2010', '2011', '2012', '2014', '2016')

biomass = melt(biomass)
colnames(biomass) = c('plot', 'transcto', 'dbh.class', 'data', 'biomass')

biomass$transcto = as.character(biomass$transcto)
biomass$plot = as.character(biomass$plot)
biomass$dbh.class = as.character(biomass$dbh.class)

#Add plot info
biomass$plot[biomass$plot == "A"] <- c("Controle")
biomass$plot[biomass$plot == "B"] <- c("B3yr")
biomass$plot[biomass$plot == "C"] <- c("B1yr")

#Convert dbh.class in transect width in meters
#biomass$dbh.class[biomass$dbh.class == "C10"] <- c("5")
#biomass$dbh.class[biomass$dbh.class == "C20"] <- c("20")
#biomass$dbh.class[biomass$dbh.class == "C40A"] <- c("50")
#biomass$dbh.class[biomass$dbh.class == "C40B"] <- c("50")

#biomass$dbh.class = as.numeric(biomass$dbh.class)

#Summary data
biomass = biomass %>%
  group_by(plot, transcto, dbh.class, data) %>% 
  summarise(biomass = sum(biomass, na.rm = TRUE))
colnames(biomass) = c('parcela', 'dist', 'dbh.class', 'data', 'biomass')

#Part 3 - Export data as CSV ================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

#write.table(biomass, "Biomass_full_tang.csv", row.names = F, sep = ",")
