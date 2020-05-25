#Exploratory Analysis and Plots
#Blowdown Data (AREA-1)
#Eduardo Q Marques 25-05-2020

library(ggplot2)
library(tidyverse)
library(reshape2)

#Load data
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Area1-plot/Campo vento')

df = read.csv("storm_data_full_B.csv", sep = ",")

#Resume data
df = df[,c(3,4,5,6,7,8,9,10,11,12)]

#Summary information
summary(df)

#Data transformation to exploration

scar = df$cicatriz+df$alt_cic+df$altura_quebra #Broken by scar? presence x altscar x altbroken


#Plot data
#Most popular plot down on wind
ggplot(df, aes(x=parcela))+
  geom_bar(position = "dodge")


#Most popular trees down on wind
ggplot(df, aes(x=nomecomum))+
  geom_bar(position = "dodge")

ggplot(df, aes(x=nomecomum, fill=parcela))+
  geom_bar(position = "dodge")

ggplot(df, aes(x=nomecomum, fill=tipo_de_dano))+
  geom_bar(position = "dodge")


#Kind of damage data
ggplot(df, aes(x=tipo_de_dano, fill = parcela))+
  geom_bar(position = "dodge")

ggplot(df, aes(x=tipo_de_dano, fill = parcela))+
  geom_bar(position = "dodge")










