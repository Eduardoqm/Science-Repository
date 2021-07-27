
library(tidyverse)
library(reshape2)
library(ggplot2)

#Data ======================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

df$year = as.numeric(df$year)
df$index = as.character(df$index)
df$treat = as.factor(df$treat)

ggplot(pssr, aes(x=year, y=x3))+
  stat_summary(geom="line", fun.y="mean", size = 1.5, aes(group=treat))

df %>% 
  na.omit() %>% 
  filter(index == "wbi") %>% 
  mutate(value = (value - mean(value))/(2*sd(value))) %>% #In SD units
  #mutate(value = (value - mean(value))) %>%
  ggplot(aes(x=year, y=value, color = treat))+
  #geom_jitter(alpha = 0.03)+
  stat_summary(geom="line", fun.y="mean", size = 1.5, aes(group=treat))+
  theme_bw()























































x <- rnorm(100, mean = 10, sd = 3)
plot(x)
hist(x)
x2 = x - mean(x)
hist(x2)
x3 = (x - mean(x))/(2*sd(x))
hist(x3)
x3 = (x - mean(x))/(1*sd(x))
hist(x3)
plot(1:length(x), x2)
plot(1:length(x), x2, type = "b")
plot(1:length(x), x3, type = "b")
plot(1:length(x), x3+rnorm(length(x)), type = "b", col = "red")
plot(1:length(x), x3, type = "b")
lines(1:length(x), x3+rnorm(length(x)), type = "b", col = "red")


library(tidyverse)
library(reshape2)
library(ggplot2)

#Data ======================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

df$year = as.numeric(df$year)
df$index = as.character(df$index)
df$treat = as.factor(df$treat)

pssr = df %>%
  filter(index == "pssr") %>% 
  filter(treat == "b3yr")

pssr = na.omit(pssr)

plot(pssr$value)
hist(pssr$value)
pssr$x2 = (pssr$value - mean(pssr$value))
hist(pssr$x2)
pssr$x3 = (pssr$value - mean(pssr$value))/(2*sd(pssr$value))
hist(pssr$x3)
pssr$x4 = (pssr$value - mean(pssr$value))/(1*sd(pssr$value))
hist(pssr$x4)
plot(1:length(pssr$value), pssr$x2)
plot(1:length(pssr$value), pssr$x2, type = "b")
plot(1:length(pssr$value), pssr$x3, type = "b")
plot(1:length(pssr$value), pssr$x3+rnorm(length(pssr$value)), type = "b", col = "red")
plot(1:length(pssr$value), pssr$x3, type = "b")
lines(1:length(pssr$value), pssr$x3+rnorm(length(pssr$value)), type = "b", col = "red")


ggplot(pssr, aes(x=year, y=x3))+
  stat_summary(geom="line", fun.y="mean", size = 1.5, aes(group=treat))



df %>% 
  na.omit() %>% 
  filter(index == "wbi") %>% 
  mutate(value = (value - mean(value))/(2*sd(value))) %>% #In SD units
  #mutate(value = (value - mean(value))) %>%
  ggplot(aes(x=year, y=value, color = treat))+
  #geom_jitter(alpha = 0.03)+
  stat_summary(geom="line", fun.y="mean", size = 1.5, aes(group=treat))+
  theme_bw()





