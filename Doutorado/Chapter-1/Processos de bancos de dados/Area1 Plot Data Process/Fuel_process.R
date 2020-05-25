#Organize Litterfall by Edge and Core on Area-1

#Eduardo Q Marques 13/02/2020

library(tidyverse)
library(reshape2)

#Data Bank ==================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Area1-plot/Fogo")

fuel = read.csv("Combustivel_Brown_04_10_Area1.csv", sep = ",", header = TRUE)

#Part 1 - Organize data =====================================================================
fuel = fuel %>% 
  filter(measure == "prf") %>% 
  mutate(fuel = NI06 + NI25 + NI76) 

fuel = fuel[,c(1,2,7)]

#Separete Lines and Transects
fuel$linha <- as.numeric(str_extract(fuel$ponto, "[0-9]+"))
fuel$dist <- as.character(str_extract(fuel$ponto, "[A-Z]+"))
fuel = fuel[,c(-1)]
colnames(fuel) = c("data", "fuel","linha", "dist")

#Add plot info
for (x in 1:10) {
  fuel$linha[fuel$linha == x] <- "controle"
}

for (x in 11:20) {
  fuel$linha[fuel$linha == x] <- "b3yr"
}

for (x in 21:31) {
  fuel$linha[fuel$linha == x] <- "b1yr"
}

#Add info edge and core
#By transect
fuel$dist[fuel$dist == "BORDA"] <- c("borda")
fuel$dist[fuel$dist == "CAPIM"] <- c("borda")
fuel$dist[fuel$dist == "AA"] <- c("borda")
fuel$dist[fuel$dist == "AB"] <- c("borda")
fuel$dist[fuel$dist == "A"] <- c("borda")
fuel$dist[fuel$dist == "B"] <- c("borda")
fuel$dist[fuel$dist == "C"] <- c("borda")
fuel$dist[fuel$dist == "D"] <- c("borda")
fuel$dist[fuel$dist == "E"] <- c("borda")
fuel$dist[fuel$dist == "F"] <- c("borda")
fuel$dist[fuel$dist == "G"] <- c("nucleo")
fuel$dist[fuel$dist == "H"] <- c("nucleo")
fuel$dist[fuel$dist == "I"] <- c("nucleo")
fuel$dist[fuel$dist == "J"] <- c("nucleo")
fuel$dist[fuel$dist == "K"] <- c("nucleo")
fuel$dist[fuel$dist == "L"] <- c("nucleo")
fuel$dist[fuel$dist == "M"] <- c("nucleo")
fuel$dist[fuel$dist == "N"] <- c("nucleo")
fuel$dist[fuel$dist == "O"] <- c("nucleo")
fuel$dist[fuel$dist == "P"] <- c("nucleo")
fuel$dist[fuel$dist == "Q"] <- c("nucleo")
fuel$dist[fuel$dist == "R"] <- c("nucleo")
fuel$dist[fuel$dist == "S"] <- c("nucleo")
fuel$dist[fuel$dist == "T"] <- c("nucleo")
fuel$dist[fuel$dist == "U"] <- c("nucleo")

#Summary data
colnames(fuel) = c("data", "fuel","parcela", "dist")

fuel = fuel %>% 
  na.omit() %>% 
  group_by(parcela, dist, data) %>% 
  summarise(fuel = sum(fuel))


#Part 2 - Export data as CSV ================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

#write.table(fuel, "Fuel_tang.csv", sep = ",")

#write.table(fuel, "Fuel_full_tang.csv", sep = ",")










