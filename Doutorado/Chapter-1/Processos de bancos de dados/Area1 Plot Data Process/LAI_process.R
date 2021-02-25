#Organize Litterfall by Edge and Core on Area-1

#Eduardo Q Marques 13/02/2020 Update: 25-02-2021

library(tidyverse)
library(reshape2)

#Data Bank ==================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Lai e Liteira")

lai = read.csv("MASTER_LAI_Area1_ABC_fev2020.csv", sep = ",", header = TRUE)

#Part 1 - Organize data =====================================================================
lai = lai[,c(1,3,11,21)]
lai$transecto = as.character(lai$transecto)

#Add plot info
for (x in 1:10) {
  lai$parcela[lai$linhas == x] <- "control"
}

for (x in 11:20) {
  lai$parcela[lai$linhas == x] <- "b3yr"
}

for (x in 21:31) {
  lai$parcela[lai$linhas == x] <- "b1yr"
}

#colnames(lai) = c("parcela", "dist", "lai", "data")

#Add info edge and core
#By transect
lai$dist[lai$transecto == "AA"] <- c("borda")
lai$dist[lai$transecto == "AB"] <- c("borda")
lai$dist[lai$transecto == "A"] <- c("borda")
lai$dist[lai$transecto == "B"] <- c("borda")
lai$dist[lai$transecto == "C"] <- c("borda")
lai$dist[lai$transecto == "D"] <- c("borda")
lai$dist[lai$transecto == "E"] <- c("borda")
lai$dist[lai$transecto == "F"] <- c("borda")
lai$dist[lai$transecto == "G"] <- c("nucleo")
lai$dist[lai$transecto == "H"] <- c("nucleo")
lai$dist[lai$transecto == "I"] <- c("nucleo")
lai$dist[lai$transecto == "J"] <- c("nucleo")
lai$dist[lai$transecto == "K"] <- c("nucleo")
lai$dist[lai$transecto == "L"] <- c("nucleo")
lai$dist[lai$transecto == "M"] <- c("nucleo")
lai$dist[lai$transecto == "N"] <- c("nucleo")
lai$dist[lai$transecto == "O"] <- c("nucleo")
lai$dist[lai$transecto == "P"] <- c("nucleo")
lai$dist[lai$transecto == "Q"] <- c("nucleo")
lai$dist[lai$transecto == "R"] <- c("nucleo")
lai$dist[lai$transecto == "S"] <- c("nucleo")
lai$dist[lai$transecto == "T"] <- c("nucleo")
lai$dist[lai$transecto == "U"] <- c("nucleo")


#Part 2 - Summary data=======================================================================
#lai = lai %>% 
  #group_by(parcela, dist, data) %>% 
  #filter(lai <= 7) #%>% 
  #summarise(lai = median(lai))

lai = filter(lai, LAI <=7)
colnames(lai)[3] = c("lai")
colnames(lai)[4] = c("year")

#Part 3 - Export data as CSV ================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

#write.table(lai, "LAI_tang.csv", sep = ",")

#write.table(lai, "LAI_full_tang.csv", sep = ",", row.names = F)











