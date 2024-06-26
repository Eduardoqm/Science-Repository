#######################################################
# Correlaion - Vegetation Indexs                      #
# Eduardo Q Marques  04/11/2019                       #
#######################################################

library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(stringr)

setwd('C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\Doutorado\\Deposito\\Banco de Dados Tanguro\\Dados para analise cap1')

hy = read.csv("Hyperion_indexs_median by plot.csv", sep = ",", header = TRUE)

#Plot data organization
#Biomass ====================================

#Transformar linha em transceto!!!! <====


biomass = read.csv("Biomassa_Tang.csv", sep = ",", header = TRUE)
biomass = biomass[,c(1:7)]
colnames(biomass) = c('plot', 'transcto', '2004', '2008', '2010', '2011', '2012')
biomass = melt(biomass)
colnames(biomass) = c('plot', 'transcto', 'data', 'biomass')


# BEM AQUI!!! <== #Transformar linha em transceto e AA, A, AB em AA!!!!


biomass = biomass %>% 
  na.omit() %>% 
  group_by(plot, transcto, data) %>% 
  summarise(biomass = sum(biomass))%>%
  ungroup()%>%
  mutate(parcela=factor(plot,labels=c("controle","b3yr","b1yr")))
  
biomass = biomass[,-1]

biomass = biomass %>% 
  mutate(parc = parcela) %>% 
  mutate(transc = transcto) %>% 
  unite(col = "local", c("parc", "transc"), sep = '_')


#Fuel ==================================
fuel = read.csv("Combustivel_Brown_Tang.csv", sep = ",", header = TRUE)

#Transformar borda, BORDA, Borda, AA, A e AB em AA!!!!

fuel = fuel %>% 
  na.omit() %>% 
  mutate(fuel = NI06 + NI25 + NI76) 

fuel = fuel[,c(1,2,7)]

#Extract numbers only
fuel$transcto <- as.character(str_extract(fuel$ponto, "[A-Z, a-z]+"))
fuel$ponto <- as.numeric(str_extract(fuel$ponto, "[0-9]+"))
fuel = na.omit(fuel)
colnames(fuel) = c("parcela", "data", "fuel", "transcto")

for (x in 1:10) {
  fuel$parcela[fuel$parcela == x] <- "controle"
}

for (x in 11:20) {
  fuel$parcela[fuel$parcela == x] <- "b3yr"
}

for (x in 21:31) {
  fuel$parcela[fuel$parcela == x] <- "b1yr"
}

fuel = fuel %>% 
  group_by(parcela, transcto, data) %>% 
  summarise(fuel = median(fuel)) %>% 
  mutate(parc = parcela) %>% 
  mutate(transc = transcto) %>% 
  unite(col = "local", c("parc", "transc"), sep = '_')

#Join everything =========================
index = index %>% 
  mutate(local2 = local) %>% 
  mutate(data2 = data) %>% 
  unite(col = "id", c("local2", "data2"), sep = '_')

biomass = biomass %>% 
  mutate(local2 = local) %>% 
  mutate(data2 = data) %>% 
  unite(col = "id", c("local2", "data2"), sep = '_')

fuel = fuel %>% 
  mutate(local2 = local) %>% 
  mutate(data2 = data) %>% 
  unite(col = "id", c("local2", "data2"), sep = '_')


index = full_join(index, biomass, by="id")
index = full_join(index, fuel, by="id")

#index = index %>% 
 # separate(col = "id", c("parcela", "data"), sep = '_')

index = index[,c(3,1,2,8,14)]
colnames(index) = c("Parcela", "NDII", "LWVI2", "Biomass", "Fuel")

index = na.omit(index)

ggpairs(index)

















#Struture
struc = hy[,c(2, 4, 10, 12, 18, 19, 21)]

struc = struc %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')

biomass = biomass %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')

lai = lai %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')

fuel = fuel %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')


struc = full_join(struc, biomass, by="id")
struc = full_join(struc, lai, by="id")
struc = full_join(struc, fuel, by="id")

struc = struc %>% 
  separate(col = "id", c("parcela", "data"), sep = '_')

ggpairs(struc[,-2])

#Biochemistry
bioc = hy[,c(2,3,5,6,9,11,14,15,17,20,21)]

bioc = bioc %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')

bioc = full_join(bioc, biomass, by="id")
bioc = full_join(bioc, lai, by="id")
bioc = full_join(bioc, fuel, by="id")

bioc = bioc %>% 
  separate(col = "id", c("parcela", "data"), sep = '_')

ggpairs(bioc[,-2])

#Physiology
phy = hy[,c(2,13,16,21)]

phy = phy %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')

phy = full_join(phy, biomass, by="id")
phy = full_join(phy, lai, by="id")
phy = full_join(phy, fuel, by="id")

phy = phy %>% 
  separate(col = "id", c("parcela", "data"), sep = '_')

ggpairs(phy[,-2])

#Fire
fir = hy[,c(2,7,8,21)]

fir = fir %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')

fir = full_join(fir, biomass, by="id")
fir = full_join(fir, lai, by="id")
fir = full_join(fir, fuel, by="id")

fir = fir %>% 
  separate(col = "id", c("parcela", "data"), sep = '_')

ggpairs(fir[,-2])







