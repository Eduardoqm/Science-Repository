####################################
# Time Series of Area-1 field data #
#                                  #
# Eduardo Q Marques 25-02-2021     #
####################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Data -----------------------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

lai = read.csv( "LAI_full_tang.csv", sep = ",")
litt = read.csv( "Liteira_full_tang.csv", sep = ",")
bmas = read.csv( "Biomass_full_tang.csv", sep = ",")

#Modify some elements -------------------------------------------------------------------
#LAI
lai$parcela = as.character(lai$parcela)
lai$parcela[lai$parcela == "control"] <- c("Controle")
lai$parcela[lai$parcela == "b3yr"] <- c("B3yr")
lai$parcela[lai$parcela == "b1yr"] <- c("B1yr")
lai = na.omit(lai)
lai$year = as.character(lai$year)
colnames(lai)[3:5] = c("LAI", "Ano", "Parcela")

#Litterfall
litt$parcela = as.character(litt$parcela)
litt$parcela[litt$parcela == "control"] <- c("Controle")
litt$parcela[litt$parcela == "b3yr"] <- c("B3yr")
litt$parcela[litt$parcela == "b1yr"] <- c("B1yr")
litt = na.omit(litt)
litt$year = as.character(litt$year)
colnames(litt)[1] = c("Parcela")

#Biomass
colnames(bmas) = c("Parcela", "dist", "DBH", "Ano", "biomass")
bmas$Ano = as.character(bmas$Ano)

bmas = bmas %>%
  group_by(Parcela, Ano) %>% 
  summarise(biomass = sum(biomass))

#Plot boxplots ---------------------------------------------------------------------------
eqm = c("orange", "red", "blue") #My color palette

#LAI
ggplot(lai, aes(x=Ano, y=LAI, fill = Parcela))+
  geom_boxplot(alpha = 0.7)+
  theme_bw()+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))

#Litterfall
ggplot(litt, aes(x=year, y=lit_ton_hec, fill = Parcela))+
  geom_boxplot(alpha = 0.7)+
  theme_bw()+
  labs(y = "Liteira Ton/hec", x = "Ano")+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))

#Biomass
ggplot(bmas, aes(x=Ano, y=biomass, fill = Parcela))+
  #geom_boxplot(alpha = 0.7)+
  geom_bar(position="dodge", stat="identity", alpha = 0.7)+
  theme_bw()+
  labs(y = "Biomassa Ton/Tratamento", x = "Ano")+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))












