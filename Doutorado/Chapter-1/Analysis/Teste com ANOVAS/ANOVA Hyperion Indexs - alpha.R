#ANOVA of Hyperion Indexs

#Eduardo Q Marques 27-07-2021

library(tidyverse)
library(reshape2)
library(ggpubr)
library(rstatix)
library(car)

#Data preparation
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

df$year = as.factor(df$year)

df2 = df[,c(2,6:9)]

df2 = df2 %>% filter(index == "ndvi") %>% na.omit()

ggboxplot(df2, x = "year", y = "value",
  color = "treat", palette = "jco")

#Test normality and homogenity
shapiro.test(df2$value)
leveneTest(value~year, data = df2)

#Create QQ plot for each cell of design
ggqqplot(selfesteem2, "score", ggtheme = theme_bw()) +
  facet_grid(treatment ~ time, labeller = "label_both")


model <- aov(value~year:treat, data = df2, paried = F)
model

TukeyHSD(model, wich = "year")

