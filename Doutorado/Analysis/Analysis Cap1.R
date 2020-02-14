#Analysis Cap-1 Hyperion Indexs on Fire
#By: Eduardo Q Marques 04-12-2019

library(tidyverse)
library(reshape2)
library(GGally)

#Load data
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

hy = read.csv("Hyperion_indexs_median by plot_Clean Vs.csv", sep = ",", header = TRUE)
biomass = read.csv("Biomassa_Tang.csv", sep = ",", header = TRUE)
lai = read.csv("LAI_Area1_Tang.csv", sep = ",", header = TRUE)
fuel = read.csv("Combustivel_Brown_Tang.csv", sep = ",", header = TRUE)
litt  = read.csv("Liteira_Area1_Tang.csv", sep = ",", header = TRUE)
#fire = read.csv("Fire.csv", sep = ",", header = TRUE) #Make the fire intensity!


#Data organization
#Biomass ====================================
#biomass = biomass[,c(1:7)]#Select date
colnames(biomass) = c('plot', 'transcto', '2004', '2008', '2010', '2011', '2012', '2014')
biomass = melt(biomass)
colnames(biomass) = c('plot', 'transcto', 'data', 'biomass')
biomass$transcto = as.character(biomass$transcto)
biomass$plot = as.character(biomass$plot)

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

#Summary data
biomass = biomass %>%
  group_by(plot, transcto, data) %>% 
  summarise(biomass = sum(biomass, na.rm = TRUE))
colnames(biomass) = c('parcela', 'dist', 'data', 'biomass')



#LAI ====================================
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

#Summary data
lai = lai %>% 
  group_by(parcela, dist, data) %>% 
  summarise(lai = median(lai)) %>% 
  filter(data == 2005:2012)



#Fuel ==================================
fuel = fuel %>% 
  na.omit() %>% 
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
  summarise(fuel = median(fuel))



#Litterfall ========================
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

#Summary data
litt = litt %>% 
  na.omit() %>% 
  group_by(parcela, dist, data) %>% 
  summarise(litt = mean(litt)) %>% 
  filter(data == 2004:2012)

#Join Data =========================
#struc = hy[,c(2, 4, 10, 12, 18, 19, 21)]

hy = hy %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

#biomass = as.data.frame(biomass)#To work
#biomass = biomass %>% 
#  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

lai = lai %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

fuel = fuel %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

litt = litt %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')


df = hy
#df = full_join(df, biomass, by="id")
df = full_join(df, lai, by="id")
df = full_join(df, fuel, by="id")
df = full_join(df, litt, by="id")

df = df %>% 
  separate(col = "id", c("parcela", "data", "dist"), sep = '_')

#write.table(df, "Area1_data_edge_core.csv", sep = ",")
#Correlation Matrix ======================
ggpairs(df)

#GLM and Dredge

