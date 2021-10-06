####################################
# Time Series of Area-1 field data #
# (Relative Difference)            #
# Eduardo Q Marques 04-10-2021     #
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
colnames(lai)[3:5] = c("LAI", "Ano", "Tratamento")

#Litterfall
litt$parcela = as.character(litt$parcela)
litt$parcela[litt$parcela == "control"] <- c("Controle")
litt$parcela[litt$parcela == "b3yr"] <- c("B3yr")
litt$parcela[litt$parcela == "b1yr"] <- c("B1yr")
litt = na.omit(litt)
colnames(litt)[1] = c("Tratamento")

#Biomass
colnames(bmas) = c("Tratamento", "dist", "DBH", "Ano", "biomass")

bmas_m = bmas %>%
  group_by(Tratamento, Ano) %>% 
  summarise(biomass = mean(biomass))

#Mean time series =================================================
lai_m = lai %>%  
  group_by(Ano, Tratamento) %>% 
  summarise(LAI = mean(LAI)) %>% 
  ungroup()

litt_m = litt %>%  
  group_by(year, Tratamento) %>% 
  summarise(lit_ton_hec = mean(lit_ton_hec)) %>% 
  ungroup()
colnames(litt_m) = c("Ano", "Tratamento", "Liteira")


ggplot(lai_m, aes(x=Ano, y=LAI, color = Tratamento))+
  geom_line(aes(group=Tratamento), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Tratamento))

ggplot(litt_m, aes(x=Ano, y=Liteira, color = Tratamento))+
  geom_line(aes(group=Tratamento), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Tratamento))

ggplot(bmas_m, aes(x=Ano, y=biomass, color = Tratamento))+
  geom_line(aes(group=Tratamento), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Tratamento))

#Relative Difference ===============================================
lai_crt = filter(lai_m, Tratamento == "Controle")
lai_b3yr = filter(lai_m, Tratamento == "B3yr")
lai_b1yr = filter(lai_m, Tratamento == "B1yr")

lai_b3yr$LAI = 100 - ((lai_b3yr$LAI*100)/lai_crt$LAI)
lai_b1yr$LAI = 100 - ((lai_b1yr$LAI*100)/lai_crt$LAI)
lai_diff = rbind(lai_b3yr, lai_b1yr)


litt_crt = filter(litt_m, Tratamento == "Controle")
litt_b3yr = filter(litt_m, Tratamento == "B3yr")
litt_b1yr = filter(litt_m, Tratamento == "B1yr")

litt_b3yr$Liteira = 100 - ((litt_b3yr$Liteira*100)/litt_crt$Liteira)
litt_b1yr$Liteira = 100 - ((litt_b1yr$Liteira*100)/litt_crt$Liteira)
litt_diff = rbind(litt_b3yr, litt_b1yr)


ggplot()+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(data = litt_diff, aes(x=Ano, y=Liteira, col = "Liteira"), size = 1.5, alpha = 0.8)+
  geom_line(data = lai_diff, aes(x=Ano, y=LAI, col = "LAI"), size = 1.5, alpha = 0.8)+
  facet_grid(rows = vars(Tratamento))+
  scale_color_manual(values = c("green","blue"))+
  labs(y = "Diferença em relação o Controle (%)")+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14))











