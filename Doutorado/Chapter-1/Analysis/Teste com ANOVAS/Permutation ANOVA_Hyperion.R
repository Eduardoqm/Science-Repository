#########################################################
# Hyperion Time series Permutation ANOVA                #
#                                                       #
# Eduardo Q Marques 17-02-2021                          #
#########################################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(mgcv)
library(visreg)
library(extrafont)
library(lmPerm)
library(FSA)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Data ============================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
df = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')

df2 = df %>% 
  na.omit() %>% 
  filter(index == "pssr") %>% 
  filter(parcela == "control")
df2$year = as.factor(df2$year)

shapiro.test(df2$value)

kruskal.test(value~year, data=df2)

summary(aovp(value~year, data=df2))

dunnTest(value~year, data=df2)

model = aovp(value~year, data=df2)

TukeyHSD(model)

boxplot(value~year, data=df2,
        xlab="Year",
        ylab = "PSSR values",
        frame.plot=F, cex=1, cex.axis=1, cex.lab=1)



