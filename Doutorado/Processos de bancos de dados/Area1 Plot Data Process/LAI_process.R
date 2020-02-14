#Organize Litterfall by Edge and Core on Area-1

#Eduardo Q Marques 13/02/2020

library(tidyverse)
library(reshape2)

#Data Bank ==================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Area1-plot/Tanguro Parcela")

lai = read.csv("MASTER_LAI_Area1_ABC_out2017.csv", sep = ",", header = TRUE)

#Part 1 - Organize data =====================================================================
lai = lai[,c(1,3,11,21)]
lai$transecto = as.character(lai$transecto)

#Add plot info
for (x in 1:10) {
  lai$linhas[lai$linhas == x] <- "controle"
}

for (x in 11:20) {
  lai$linhas[lai$linhas == x] <- "b3yr"
}

for (x in 21:31) {
  lai$linhas[lai$linhas == x] <- "b1yr"
}

colnames(lai) = c("parcela", "dist", "lai", "data")

#Add info edge and core
#By transect
lai$dist[lai$dist == "AA"] <- c("borda")
lai$dist[lai$dist == "AB"] <- c("borda")
lai$dist[lai$dist == "A"] <- c("borda")
lai$dist[lai$dist == "B"] <- c("borda")
lai$dist[lai$dist == "C"] <- c("borda")
lai$dist[lai$dist == "D"] <- c("borda")
lai$dist[lai$dist == "E"] <- c("borda")
lai$dist[lai$dist == "F"] <- c("borda")
lai$dist[lai$dist == "G"] <- c("nucleo")
lai$dist[lai$dist == "H"] <- c("nucleo")
lai$dist[lai$dist == "I"] <- c("nucleo")
lai$dist[lai$dist == "J"] <- c("nucleo")
lai$dist[lai$dist == "K"] <- c("nucleo")
lai$dist[lai$dist == "L"] <- c("nucleo")
lai$dist[lai$dist == "M"] <- c("nucleo")
lai$dist[lai$dist == "N"] <- c("nucleo")
lai$dist[lai$dist == "O"] <- c("nucleo")
lai$dist[lai$dist == "P"] <- c("nucleo")
lai$dist[lai$dist == "Q"] <- c("nucleo")
lai$dist[lai$dist == "R"] <- c("nucleo")
lai$dist[lai$dist == "S"] <- c("nucleo")
lai$dist[lai$dist == "T"] <- c("nucleo")
lai$dist[lai$dist == "U"] <- c("nucleo")


#Part 2 - Summary data=======================================================================
lai = lai %>% 
  group_by(parcela, dist, data) %>% 
  filter(lai <= 7) %>% 
  summarise(lai = median(lai))


#Part 3 - Export data as CSV ================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

#write.table(lai, "LAI_tang.csv", sep = ",")












