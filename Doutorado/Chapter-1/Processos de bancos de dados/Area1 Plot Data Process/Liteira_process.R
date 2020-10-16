#Organize Litterfall by Edge and Core on Area-1

#Eduardo Q Marques 13/02/2020 Update: 13-10-2020

library(tidyverse)
library(reshape2)

#Data Bank ==================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Area1-plot/Tanguro Parcela")

litt = read.csv("1_master_liteira_area_1_jun2019.csv", sep = ",", header = TRUE)

#Part 1 - Organize data =====================================================================
litt = litt[,c(1,3,8,10,13)]
colnames(litt) = c("parcela", "trans_line","transecto", "data", "litt")
litt$transecto = as.character(litt$transecto)
litt$parcela = as.character(litt$parcela)

#Add plot info
litt$parcela[litt$parcela == "A"] <- c("control")
litt$parcela[litt$parcela == "B"] <- c("b3yr")
litt$parcela[litt$parcela == "C"] <- c("b1yr")

#Add info edge and core
#By transect
litt$dist[litt$transecto == "Bo"] <- c("borda")
litt$dist[litt$transecto == "AA"] <- c("borda")
litt$dist[litt$transecto == "AB"] <- c("borda")
litt$dist[litt$transecto == "A"] <- c("borda")
litt$dist[litt$transecto == "B"] <- c("borda")
litt$dist[litt$transecto == "C"] <- c("borda")
litt$dist[litt$transecto == "D"] <- c("borda")
litt$dist[litt$transecto == "E"] <- c("borda")
litt$dist[litt$transecto == "F"] <- c("borda")
litt$dist[litt$transecto == "G"] <- c("nucleo")
litt$dist[litt$transecto == "H"] <- c("nucleo")
litt$dist[litt$transecto == "I"] <- c("nucleo")
litt$dist[litt$transecto == "J"] <- c("nucleo")
litt$dist[litt$transecto == "K"] <- c("nucleo")
litt$dist[litt$transecto == "L"] <- c("nucleo")
litt$dist[litt$transecto == "M"] <- c("nucleo")
litt$dist[litt$transecto == "N"] <- c("nucleo")
litt$dist[litt$transecto == "O"] <- c("nucleo")
litt$dist[litt$transecto == "P"] <- c("nucleo")
litt$dist[litt$transecto == "Q"] <- c("nucleo")
litt$dist[litt$transecto == "R"] <- c("nucleo")
litt$dist[litt$transecto == "S"] <- c("nucleo")
litt$dist[litt$transecto == "T"] <- c("nucleo")
litt$dist[litt$transecto == "U"] <- c("nucleo")


#Part 2 - Calculate tons by hectare==========================================================
#We know that the basket is 60x80 cm in size, so we calculate its area and convert it from cm2 to hectares. The weight of the litter is just to convert grams to tons.
litt$hec = as.numeric(((60*80)/10000)/10000)
litt$ton = as.numeric(litt$litt/1000000)

litt = litt %>% 
  na.omit() %>% 
  group_by(parcela, trans_line, transecto, dist, data) %>% 
  summarise(ton = sum(ton), hec = sum(hec))

#Convert in Ton/Hectares
litt$lit_ton_hec = as.numeric(litt$ton/litt$hec)
litt = litt[,c(-6,-7)]

colnames(litt)[5] = c("year")


#Part 3 - Export data as CSV ================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

#write.table(litt, "Liteira_tang.csv", sep = ",")


#write.table(litt, "Liteira_full_tang.csv", sep = ",")









