# Linear Model hypespectral VIs

# Eduardo Q Marques 21-07-2021

library(tidyverse)
library(reshape2)

#Data ======================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

df$year = as.numeric(df$year)
df$index = as.character(df$index)
df$treat = as.factor(df$treat)

x11()

#Without date as factor
lmx = function(z){
  w = df %>% filter(index == z)
  w = lm(value ~ treat + year, data = w)
  print(summary(w))
  layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))
  plot(w)
  hist(w$residuals, main="Histogram of Residuals")
}

lmx("evi2")
lmx("ndvi")
lmx("ndii")
lmx("vig")
lmx("vari")
lmx("nirv")
lmx("lwvi2")
lmx("msi")
lmx("ndwi")
lmx("pssr")
lmx("psri")
lmx("sipi")
lmx("wbi")
lmx("pri")
lmx("rendvi")
lmx("nbr")
lmx("nbr2")

#With date as factor
lmx2 = function(z){
  w = df %>% filter(index == z)
  w$year = as.factor(w$year)
  w = lm(value ~ treat + year, data = w)
  print(summary(w))
  layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))
  plot(w)
  hist(w$residuals, main="Histogram of Residuals")
}

lmx2("evi2")
lmx2("ndvi")
lmx2("ndii")
lmx2("vig")
lmx2("vari")
lmx2("nirv")
lmx2("lwvi2")
lmx2("msi")
lmx2("ndwi")
lmx2("pssr")
lmx2("psri")
lmx2("sipi")
lmx2("wbi")
lmx2("pri")
lmx2("rendvi")
lmx2("nbr")
lmx2("nbr2")
