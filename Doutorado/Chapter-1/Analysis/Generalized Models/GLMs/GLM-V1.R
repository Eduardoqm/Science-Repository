#GLM tests

#Eduardo Q Marques 23-02-2021

library(tidyverse)
library(reshape2)

#Data ============================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
df = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')

#Ask-1
#Which vegetation indices allow you to detect variations in vegetation reflectance under water stress conditions caused by prolonged dry periods in 2007 and 2010?

#Select Indices in PCA axis and only in control treatment
df2 = df %>% 
  na.omit() %>% 
  filter(index %in% c("pssr", "ndwi", "evi2", "pri", "psri","msi")) %>% 
  filter(parcela == "control")

#Include column of Dry or Normal year
df2$cond = df2$year
df2$cond[df2$cond != 2010] <- c("Normal")
df2$cond[df2$cond == 2010] <- c("Dry")
df2$cond = as.factor(df2$cond)
df2$index = as.factor(df2$index)
df2$year = as.numeric(df2$year)
#df2$value2 = scale(df2$value)

#Run GLMs
m1 = glm(value~cond+index+cond:index, data = df2, family = gaussian(link = "identity"))
summary(m1)

m2 = glm(value~cond+index, data = df2, family = gaussian(link = "identity"))
summary(m2)

m3 = glm(value~cond+index+year+year:index+cond:index, data = df2, family = gaussian(link = "identity"))
summary(m3)

#Test difference between models
anova(m1, m2, m3, test = "Chisq")
#P < 0.05, so are differents models. We choose the complex model (m3).






















