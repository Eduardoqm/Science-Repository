#########################################################
# Hyperion Time series for mean and relative difference #
#                                                       #
# Eduardo Q Marques 10-02-2021                          #
#########################################################

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
df = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')

#Modify Data =====================================================
df$year = as.numeric(df$year)
df$index = as.character(df$index)
df$parcela = as.character(df$parcela)
df$value = as.numeric(df$value)

#Modify elements of dataframe
df$index = toupper(df$index)
df$index[df$index == "NIRV"] <- c("NIRv")

df$parcela[df$parcela == "control"] <- c("Controle")
df$parcela[df$parcela == "b3yr"] <- c("B3yr")
df$parcela[df$parcela == "b1yr"] <- c("B1yr")

eqm = c("orange", "red", "blue") #My color palette

#Mean time series =================================================
df_m = df %>% 
  na.omit() %>% 
  group_by(year, parcela, index) %>% 
  summarise(value = mean(value))

df_m2 = df_m #To not have problem in diff...I dont know why!
colnames(df_m2) = c("Ano", "Tratamento", "Indice", "Valor")



df_m2 %>% 
  filter(Indice != "ARI") %>% 
  filter(Indice != "EVI") %>% 
  ggplot(aes(x=Ano, y=Valor, color = Tratamento))+
  #geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
   #         fill = "blue", color = NA, alpha = 0.003)+
  #annotate("text", x = 2007.5, y = 0.3, size = 4, label = "Fire period")+
  geom_line(aes(group = Tratamento), size = 1.5, alpha = 0.7)+
  #facet_grid(rows = vars(Indice), scales = "free")+
  facet_wrap(~Indice, scales = "free")+
  theme_bw()+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))



#Diference by sbtraction ===========================================
#Calculate difference in relation of control
df_crt = filter(df_m, parcela == "Controle")
df_b3yr = filter(df_m, parcela == "B3yr")
df_b1yr = filter(df_m, parcela == "B1yr")

df_b3yr$value = 100 - ((df_b3yr$value*100)/df_crt$value)
df_b1yr$value = 100 - ((df_b1yr$value*100)/df_crt$value)
df_diff = rbind(df_b3yr, df_b1yr)
#df_diff2 = df_diff
colnames(df_diff) = c("Ano", "Tratamento", "Indice", "Valor")
#df_diff2$Valor2 = 100 - (df_diff2$Valor)


#df_crt = filter(df_m, parcela == "Controle")
#df_b3yr = filter(df_m, parcela == "B3yr")
#df_b1yr = filter(df_m, parcela == "B1yr")

#df_b3yr$value = df_b3yr$value - df_crt$value
#df_b1yr$value = df_b1yr$value - df_crt$value
#df_diff = rbind(df_b3yr, df_b1yr)

#Percent Calculation
#df_diff2 = df_diff

#Normalized indices
#normal = df_diff2 %>% 
#  filter(index != "ARI") %>% 
#  filter(index != "WBI") %>% 
#  filter(index != "PSSR")

#normal$Valor2 = normal$value*100 #To be percentege

#No normalized indices
#anormal = df_diff2 %>% 
#  filter(index %in% c("ARI", "WBI", "PSSR"))

#anor_crt = df_crt %>% 
#  filter(index %in% c("ARI", "WBI", "PSSR"))

#anor_crt = rbind(anor_crt, anor_crt)

#anormal$Valor2 = (anormal$value*100)/anor_crt$value

#df_diff3 = rbind(normal, anormal)
#colnames(df_diff3) = c("Ano", "Tratamento", "Indice", "Valor", "Valor2")

#Adjust to plot
df_diff3 = df_diff %>% 
  filter(Indice != "ARI") %>% 
  filter(Indice != "EVI")
#df$Indice[df$Indice == "EVI2"] <- c("EVI")
  

ggplot(df_diff3, aes(x=Ano, y=Valor, color = Tratamento))+
  #geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            #fill = "blue", color = NA, alpha = 0.009)+
  geom_line(aes(group = Tratamento), size = 1.5, alpha = 0.7)+
  #facet_grid(rows = vars(Indice), scales = "free")+
  facet_wrap(~Indice, scales = "free")+
  labs(y = "Diferença em relação o Controle (%)")+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))


#Abstract difference
#All
summary(df_diff3)

report = function(x,y){
  summary(df_diff3 %>% filter(Tratamento == y) %>% filter(Indice == x))
}

#NDVI
report("PSSR", "B3yr")
report("PSSR", "B1yr")

#EVI
report("RENDVI", "B3yr")
report("RENDVI", "B1yr")

#NDII
report("NDII", "B3yr")
report("NDII", "B1yr")

#VIG
report("VIG", "B3yr")
report("VIG", "B1yr")
