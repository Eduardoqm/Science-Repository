# Trabalho do Malors /21/08/2019

library(bfast)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

setwd('C:/Users/Eduardo Q Marques/Documents')
evi <- read.csv("evi_unweighted_median.csv", header = TRUE, sep = ",")
View(evi)


evi$orden = c(1:28) #Aui inclui uma ordem numerica para ele plotar na ordem certa


#Esse plot just plot a um por vez (0-15m)
ggplot(evi, aes(orden, X0.15m)) +
  geom_line() +
  labs(fill= "Plot",x="Data",y="EVI")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#Aqui arrumei os dados para ele plotar todos juntos
gg <- evi[,c(2:7)]
gg <- melt(gg, id.vars="orden")
colnames(gg) = c('orden', 'Gradients', 'evi')

#Aqui foi feito o plot de todos juntos
ggplot(gg, aes(orden,evi, col=Gradients))+ 
  geom_line(aes(group=Gradients), size = 1)+
  geom_point()+
  geom_vline(xintercept = 11, color = "red", linetype = "dashed")+
  #stat_smooth(aes(group=gradiente), method = "loess", formula = y ~ x, size = 0.5, alpha = 0)+
  labs(fill= "Plot",x="Date",y="EVI")+
  theme_minimal()
