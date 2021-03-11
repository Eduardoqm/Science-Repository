#=============================#
#Correlation Landast Indices  #
#                             #
#Eduardo Q Marques 08-03-2020 #
#=============================#

library(stats)
library(tidyverse)
library(reshape2)
library(GGally)
library(ggcorrplot)
library(ggpubr)
library(FactoMineR)
library(factoextra)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Data =======================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Transform data for analysis ================================================================
df2 = df[c(5,7)]

#Transpor variables in columns
transp = function(x){
  z = df2 %>% 
    filter(index == x)
  z = as.data.frame(z[,c(2)])
}

evi = transp("evi2");colnames(evi) = c("EVI")
ndvi = transp("ndvi");colnames(ndvi) = c("NDVI")
vig = transp("vig");colnames(vig) = c("VIG")
ndii = transp("ndii");colnames(ndii) = c("NDII")

#Join everything
df3 = cbind(df$treat,evi,ndvi,vig,ndii)
colnames(df3)[1] = c("Parcela")

#Change names
df3$Parcela = as.character(df3$Parcela)
df3$Parcela[df3$Parcela == "b1yr"] <- "B1yr"
df3$Parcela[df3$Parcela == "b3yr"] <- "B3yr"
df3$Parcela[df3$Parcela == "control"] <- "Controle"

#Correlation by ggally ======================================================================
df4 = df3[,c(-1)]
ggcorr(df4, label = T)

#Use ggpairs
p = ggpairs(df3, columns = 2:5, ggplot2::aes(color=Parcela))+
  theme_bw()

for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("orange", "red", "blue")) +
      scale_color_manual(values=c("orange", "red", "blue"))  
  }
}
p  


















