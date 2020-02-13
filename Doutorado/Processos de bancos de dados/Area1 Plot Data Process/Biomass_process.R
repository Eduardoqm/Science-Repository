#Organize biomass by Edge and Core on Area-1

#Eduardo Q Marques 13/02/2020

library(tidyverse)
library(reshape2)

#Data Bank ==================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Area1-plot/Fogo")

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
biomass$plot[biomass$plot == "A"] <- c("controle")
biomass$plot[biomass$plot == "B"] <- c("b3yr")
biomass$plot[biomass$plot == "C"] <- c("b1yr")

#Add info edge and core
#By transect
biomass$transcto[biomass$transcto == "AA"] <- c("borda")
biomass$transcto[biomass$transcto == "AB"] <- c("borda")
biomass$transcto[biomass$transcto == "A"] <- c("borda")
biomass$transcto[biomass$transcto == "B"] <- c("borda")
biomass$transcto[biomass$transcto == "C"] <- c("borda")
biomass$transcto[biomass$transcto == "D"] <- c("borda")
biomass$transcto[biomass$transcto == "E"] <- c("borda")
biomass$transcto[biomass$transcto == "F"] <- c("borda")
biomass$transcto[biomass$transcto == "G"] <- c("nucleo")
biomass$transcto[biomass$transcto == "H"] <- c("nucleo")
biomass$transcto[biomass$transcto == "I"] <- c("nucleo")
biomass$transcto[biomass$transcto == "J"] <- c("nucleo")
biomass$transcto[biomass$transcto == "K"] <- c("nucleo")
biomass$transcto[biomass$transcto == "L"] <- c("nucleo")
biomass$transcto[biomass$transcto == "M"] <- c("nucleo")
biomass$transcto[biomass$transcto == "N"] <- c("nucleo")
biomass$transcto[biomass$transcto == "O"] <- c("nucleo")
biomass$transcto[biomass$transcto == "P"] <- c("nucleo")
biomass$transcto[biomass$transcto == "Q"] <- c("nucleo")
biomass$transcto[biomass$transcto == "R"] <- c("nucleo")
biomass$transcto[biomass$transcto == "S"] <- c("nucleo")
biomass$transcto[biomass$transcto == "T"] <- c("nucleo")
biomass$transcto[biomass$transcto == "U"] <- c("nucleo")
#By edge dist
biomass$transcto[biomass$transcto == "(0,250]"] <- c("borda")
biomass$transcto[biomass$transcto == "(250,500]"] <- c("nucleo")
biomass$transcto[biomass$transcto == "(500,750]"] <- c("nucleo")
biomass$transcto[biomass$transcto == "(750,1e+03]"] <- c("nucleo")

#Convert dbh.class in transect width in meters
biomass$dbh.class[biomass$dbh.class == "C10"] <- c("2")
biomass$dbh.class[biomass$dbh.class == "C20"] <- c("5")
biomass$dbh.class[biomass$dbh.class == "C40A"] <- c("10")
biomass$dbh.class[biomass$dbh.class == "C40B"] <- c("10")
biomass$dbh.class = as.numeric(biomass$dbh.class)

#Summary data
biomass = biomass %>%
  group_by(plot, transcto, dbh.class, data) %>% 
  summarise(biomass = sum(biomass, na.rm = TRUE))
colnames(biomass) = c('parcela', 'dist', 'width', 'data', 'biomass')

#Part 2 - Calculate tons by hectare==========================================================
#We know that it has 500 meters per transect and now we have the width of the transect. We will have to multiply to have the area in m2 and transform it into hectares.
biomass$m2 = as.numeric(biomass$width*500)
biomass$hec = as.numeric(biomass$m2/10000)

#Calculate Tons/Hectares















