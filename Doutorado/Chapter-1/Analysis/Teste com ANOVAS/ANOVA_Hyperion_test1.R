#ANOVA of Hyperion Indexs

#Eduardo Q Marques 08-09-2020

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(car)
library(coin)

#Data ======================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

df$year = as.factor(df$year)
df$index = as.character(df$index)
df$treat = as.factor(df$treat)


#First ask, hidric strees on control plot
#PSSR
pssr = df %>% 
  filter(index == "pssr" & treat == "control")

#Test normality and homogenity
shapiro.test(pssr$value)
leveneTest(value~year, data = pssr)

#Try log
pssr$value_log = log10(pssr$value)
shapiro.test(pssr$value_log)
leveneTest(value_log~year, data = pssr)

#Try squared root
pssr$value_sqr = sqrt(pssr$value)
shapiro.test(pssr$value_sqr)
leveneTest(value_sqr~year, data = pssr)


#FREDMAN Test
#We will need a ID to repetitions
pssr = pssr[,c(6,7)] #Clean data

#Create ID for different years
pssr$ID[pssr$year == "2004"] <- c(1)
pssr$ID[pssr$year == "2005"] <- c(2)
pssr$ID[pssr$year == "2006"] <- c(3)
pssr$ID[pssr$year == "2008"] <- c(4)
pssr$ID[pssr$year == "2010"] <- c(5)
pssr$ID[pssr$year == "2011"] <- c(6)
pssr$ID[pssr$year == "2012"] <- c(7)

#pssr[,"ID"] = factor(pssr[,"ID"])
#pssr$year = as.factor(pssr$year)
#friedman_test(value~year|ID, pssr)

pssr$ID2  = seq(1,561, length = 561)

pssr[,"ID2"] = factor(pssr[,"ID2"])
pssr$year = as.factor(pssr$year)




friedman.test(pssr$ID2, pssr$year, pssr$value, console = TRUE)


#Simplify
pssr2 = pssr %>% 
  na.omit() %>% 
  group_by(year) %>% 
  summarise(value = mean(value))7


#Test normality and homogenity
shapiro.test(pssr2$value)
leveneTest(value~year, data = pssr2)

aov(value~year, data = pssr2, paried = TRUE)





