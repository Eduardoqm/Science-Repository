########################################################
# Landsat Time series for mean and relative difference #
#                                                      #
# Eduardo Q Marques 20-01-2021  update: 15-07-2021     #
########################################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(mgcv)
library(visreg)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Data ============================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
df = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Modify Data =====================================================
df$year = substr(df$year, 1,4)
df$year = as.numeric(df$year)
df$index = as.character(df$index)
df$treat = as.character(df$treat)

#Modify elements of dataframe
df$index[df$index == "evi2"] <- c("EVI")
df$index[df$index == "ndvi"] <- c("NDVI")
df$index[df$index == "ndii"] <- c("NDII")
df$index[df$index == "grnd"] <- c("GRND")
df$index[df$index == "nbr"] <- c("NBR")
df$index[df$index == "nbr2"] <- c("NBR2")

df$treat[df$treat == "control"] <- c("Controle")
df$treat[df$treat == "b3yr"] <- c("B3yr")
df$treat[df$treat == "b1yr"] <- c("B1yr")

eqm = c("orange", "red", "blue") #My color palette

#Resume repeat years with mean ====================================
df2 = df %>% 
  group_by(x, y, index, year, treat) %>% 
  summarise(value = mean(value))

#Smooth time series ===============================================
df_smt = df2[,c(4,5,3,6)]
df_smt$year = as.character(df_smt$year)
colnames(df_smt) = c("Ano", "Tratamento", "Indice", "Valor")

ggplot(df_smt, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento),size = 1.2, level = 0.9999999999999)+
  geom_vline(xintercept = "2004", linetype = "dashed")+
  geom_vline(xintercept = "2011", linetype = "dashed")+
  #stat_summary(geom="point", fun.y="mean", size = 2, aes(group=Tratamento))+
  facet_grid(rows = vars(Indice), scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))

#Mean time series =================================================
ggplot(df_smt, aes(x=Ano, y=Valor, color = Tratamento))+
  #geom_jitter(alpha = 0.03)+
  geom_vline(xintercept = "2004", linetype = "dashed")+
  geom_vline(xintercept = "2011", linetype = "dashed")+
  stat_summary(geom="line", fun.y="mean", size = 1.5, aes(group=Tratamento))+
  #stat_summary(geom="point", fun.y="mean", size = 2, aes(group=Tratamento))+
  facet_grid(rows = vars(Indice), scales = "free")+
  theme_bw()+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))


#Diference by sbtraction ===========================================
df_m = df2 %>% 
  group_by(year, treat, index) %>% 
  summarise(value = mean(value))

df_m2 = df_m #To not have problem in diff...I dont know why!
colnames(df_m2) = c("Ano", "Tratamento", "Indice", "Valor")

#Calculate difference in relation of control
df_crt = filter(df_m, treat == "Controle")
df_b3yr = filter(df_m, treat == "B3yr")
df_b1yr = filter(df_m, treat == "B1yr")

df_b3yr$value = 100 - ((df_b3yr$value*100)/df_crt$value)
df_b1yr$value = 100 - ((df_b1yr$value*100)/df_crt$value)
df_diff = rbind(df_b3yr, df_b1yr)
colnames(df_diff) = c("Ano", "Tratamento", "Indice", "Valor")


ggplot(df_diff, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            fill = "blue", color = NA, alpha = 0.009)+
  geom_line(aes(group = Tratamento), size = 1.5, alpha = 0.7)+
  facet_grid(rows = vars(Indice), scales = "free")+
  labs(y = "Diferença em relação o Controle (%)")+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))


ggplot(df_diff, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            fill = "blue", color = NA, alpha = 0.009)+
  geom_line(aes(group = Tratamento), size = 1.5, alpha = 0.7)+
  facet_grid(rows = vars(Indice))+
  labs(y = "Diferença em relação o Controle (%)")+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))


ggplot(df_diff, aes(x=Ano, y=Valor, color = Indice))+
  geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            fill = "red", color = NA, alpha = 0.005883)+
  #geom_vline(xintercept = 2004, linetype = "dashed")+
  #geom_vline(xintercept = 2011, linetype = "dashed")+
  geom_line(aes(group = Indice), size = 1.5, alpha = 0.95)+
  facet_grid(rows = vars(Tratamento))+
  labs(y = "Diferença em relação o Controle (%)")+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = c('#377eb8','#1b9e77','#e41a1c','darkred','#ff7f00','#4daf4a'))+
  theme(text = element_text(family = "Times New Roman", size = 14))













