# Linear Model hypespectral VIs

# Eduardo Q Marques 21-07-2021

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Data ======================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

df$year = as.numeric(df$year)
df$index = as.character(df$index)
df$treat = as.factor(df$treat)

pssr = df %>% filter(index == "pssr")

model = lm(value ~ treat + year, data = pssr)
summary(model)

layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))
plot(model)
hist(model$residuals, main="Histogram of Residuals")


#With date as factor
pssr$year = as.factor(pssr$year)

model = lm(value ~ treat + year, data = pssr)
summary(model)

layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))
plot(model)
hist(model$residuals, main="Histogram of Residuals")








