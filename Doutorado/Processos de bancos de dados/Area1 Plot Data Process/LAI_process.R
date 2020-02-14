#Organize Litterfall by Edge and Core on Area-1

#Eduardo Q Marques 13/02/2020

library(tidyverse)
library(reshape2)

#Data Bank ==================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Area1-plot/Tanguro Parcela")

lai = read.csv("MASTER_LAI_Area1_ABC_out2017.csv", sep = ",", header = TRUE)

#Part 1 - Organize data =====================================================================
litt = litt[,c(1,8,10,13)]
colnames(litt) = c("parcela", "dist", "data", "litt")
litt$dist = as.character(litt$dist)
litt$parcela = as.character(litt$parcela)

#Add plot info
litt$parcela[litt$parcela == "A"] <- c("controle")
litt$parcela[litt$parcela == "B"] <- c("b3yr")
litt$parcela[litt$parcela == "C"] <- c("b1yr")

#Add info edge and core
#By transect
litt$dist[litt$dist == "Bo"] <- c("borda")
litt$dist[litt$dist == "AA"] <- c("borda")
litt$dist[litt$dist == "AB"] <- c("borda")
litt$dist[litt$dist == "A"] <- c("borda")
litt$dist[litt$dist == "B"] <- c("borda")
litt$dist[litt$dist == "C"] <- c("borda")
litt$dist[litt$dist == "D"] <- c("borda")
litt$dist[litt$dist == "E"] <- c("borda")
litt$dist[litt$dist == "F"] <- c("borda")
litt$dist[litt$dist == "G"] <- c("nucleo")
litt$dist[litt$dist == "H"] <- c("nucleo")
litt$dist[litt$dist == "I"] <- c("nucleo")
litt$dist[litt$dist == "J"] <- c("nucleo")
litt$dist[litt$dist == "K"] <- c("nucleo")
litt$dist[litt$dist == "L"] <- c("nucleo")
litt$dist[litt$dist == "M"] <- c("nucleo")
litt$dist[litt$dist == "N"] <- c("nucleo")
litt$dist[litt$dist == "O"] <- c("nucleo")
litt$dist[litt$dist == "P"] <- c("nucleo")
litt$dist[litt$dist == "Q"] <- c("nucleo")
litt$dist[litt$dist == "R"] <- c("nucleo")
litt$dist[litt$dist == "S"] <- c("nucleo")
litt$dist[litt$dist == "T"] <- c("nucleo")
litt$dist[litt$dist == "U"] <- c("nucleo")


#Part 2 - Calculate tons by hectare==========================================================
#We know that the basket is 60x80 cm in size, so we calculate its area and convert it from cm2 to hectares. The weight of the litter is just to convert grams to tons.
litt$hec = as.numeric(((60*80)/10000)/10000)
litt$ton = as.numeric(litt$litt/1000000)

litt = litt %>% 
  na.omit() %>% 
  group_by(parcela, dist, data) %>% 
  summarise(ton = sum(ton), hec = sum(hec))

#Convert in Ton/Hectares
litt$lit_ton_hec = as.numeric(litt$ton/litt$hec)
litt = litt[,c(1,2,3,6)]


#Part 3 - Export data as CSV ================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

#write.table(litt, "Liteira_tang.csv", sep = ",")












