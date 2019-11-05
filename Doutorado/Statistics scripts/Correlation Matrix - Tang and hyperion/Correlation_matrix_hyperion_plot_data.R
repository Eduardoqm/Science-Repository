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

#All Indexs =====================================
ggpairs(hy[,-1])

#Struture
struc = hy[,c(2, 4, 10, 12, 18, 19)]
ggpairs(struc)

#Biochemistry
bioc = hy[,c(2,3,5,6,9,11,14,15,17,20)]
ggpairs(bioc)

#Physiology
phy = hy[,c(2,13,16)]
ggpairs(phy)

#Fire
fir = hy[,c(2,7,8)]
ggpairs(fir)


#Plot data organization
#Biomass ====================================
biomass = read.csv("Biomassa_Tang.csv", sep = ",", header = TRUE)
biomass = biomass[,c(1:6)]
colnames(biomass) = c('plot', '2004', '2008', '2010', '2011', '2012')
biomass = melt(biomass)
colnames(biomass) = c('plot', 'date', 'biomass')

biomass = biomass %>% 
  na.omit() %>% 
  group_by(plot, date) %>% 
  summarise(biomass = sum(biomass))%>%
  ungroup()%>%
  mutate(parcela=factor(plot,labels=c("controle","b3yr","b1yr")))
  
biomass = biomass[,-1]

#LAI ====================================
lai = read.csv("LAI_Area1_ABC_out2017.csv", sep = ",", header = TRUE)

for (x in 1:10) {
  lai$linhas[lai$linhas == x] <- "controle"
}

for (x in 11:20) {
  lai$linhas[lai$linhas == x] <- "b3yr"
}

for (x in 21:31) {
  lai$linhas[lai$linhas == x] <- "b1yr"
}

colnames(lai) = c("parcela", "lai", "date")

lai = lai %>% 
  group_by(parcela, date) %>% 
  summarise(lai = median(lai)) %>% 
  filter(date == 2005:2011)

#Fuel ==================================
fuel = read.csv("Combustivel_Brown_Tang.csv", sep = ",", header = TRUE)

fuel = fuel %>% 
  na.omit() %>% 
  mutate(fuel = NI06 + NI25 + NI76) 

fuel = fuel[,c(1,2,7)]

# extract numbers only
fuel$ponto <- as.numeric(str_extract(fuel$ponto, "[0-9]+"))
fuel = na.omit(fuel)
colnames(fuel) = c("parcela", "date", "fuel")

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
  group_by(parcela, date) %>% 
  summarise(fuel = median(fuel))

#Join everything =========================
#Struture
#struc = hy[,c(2, 4, 10, 12, 18, 19)]
#struc = melt(struc)

#biomass = biomass[,-1]
#biomass = melt(biomass)

#lai = lai[,-2]
#lai = melt(lai)

#fuel = fuel[,-2]
#fuel = melt(fuel)

struc = full_join(struc, biomass, lai, fuel)
#struc = t(struc)

ggpairs(struc, biomass, lai, fuel)









