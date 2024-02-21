#################################################
# Landsat Recovery mean and relative difference #
#                                               #
# Eduardo Q Marques 04-10-2021                  #
#################################################

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

#Resume repeat years with mean ====================================
df = df %>% 
  group_by(x, y, index, year, treat) %>% 
  summarise(value = mean(value)) %>% 
  filter(year >= 2004)

#Edge - Core separation ====================================================================
summary(df$y)
min(df$y) #-13.08343 = 1000 meters
max(df$y) #-13.07422 = 0 meters

diffy = min(df$y) - max(df$y)
df$dist = ((max(df$y) - df$y)/diffy)*1000
df$dist = abs(df$dist)

summary(df$dist)

df$dist2 = c("a")
df$dist2[df$dist <= 250] = c("Borda")
df$dist2[df$dist > 250] = c("Interior")

#Smooth time series ===============================================
df_smt = df #%>% select(x, y, index, year, treat, value)
df_smt$year = as.character(df_smt$year)
colnames(df_smt) = c("x","y","Indice","Ano","Tratamento","Valor","Dist","Dist2")

eqm = c("orange", "red", "blue") #My color palette

smtplot = ggplot(df_smt, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.5, size = 1, level = 0.9999999999999)+
  geom_vline(xintercept = "2004", linetype = "dashed")+
  geom_vline(xintercept = "2011", linetype = "dashed")+
  #stat_summary(geom="line", fun.data="mean_cl_boot", size = 0.5, linetype = "dashed", aes(group=Tratamento))+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  #stat_summary(geom="pointrange", fun.data="mean_cl_boot", alpha = 0.5, aes(group=Tratamento))+
  #stat_summary(fun.data = "mean_cl_boot", geom = "errorbar")+
  facet_grid(rows = vars(Indice), scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_manual(values = eqm)+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))

smtplot

#ggsave(filename = "Smooth_Landsat.png", plot = smtplot,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Landsat Time Series", 
#       width = 20, height = 16, units =  "cm", dpi = 300)

#Mean time series =================================================
df_m = df %>% 
  group_by(year, treat, index) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()

df_m2 = df_m #To not have problem in diff...I dont know why!
colnames(df_m2) = c("Ano", "Tratamento", "Indice", "Valor")

ggplot(df_m2, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_line(aes(group=Tratamento), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Tratamento))+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  facet_grid(rows = vars(Indice), scales = "free")+
  theme_bw()+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))


#Diference by sbtraction ===========================================
#Calculate difference in relation of control
df_crt = filter(df_m, treat == "Controle")
df_b3yr = filter(df_m, treat == "B3yr")
df_b1yr = filter(df_m, treat == "B1yr")

df_b3yr$value = 100 - ((df_b3yr$value*100)/df_crt$value)
df_b1yr$value = 100 - ((df_b1yr$value*100)/df_crt$value)
df_diff = rbind(df_b3yr, df_b1yr)
colnames(df_diff) = c("Ano", "Tratamento", "Indice","Valor")
#df_diff$Valor = abs(df_diff$Valor)

difplot = ggplot(df_diff, aes(x=Ano, y=Valor, color = Indice))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Indice), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_grid(rows = vars(Tratamento))+
  labs(y = "Diferença em relação o Controle (%)")+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  scale_color_manual(values = c('#377eb8','#1b9e77','#e41a1c','darkred','#ff7f00','#4daf4a'))+
  theme(text = element_text(family = "Times New Roman", size = 14))

difplot

#ggsave(filename = "Landsat_1985-2019_diff.png", plot = difplot,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Landsat Time Series", 
#     width = 20, height = 10, units =  "cm", dpi = 300)



#Edge and Interior facet ===============================================
#Mean time series ======================================================
df_edge = df %>% 
  filter(dist2 == "Borda") %>% 
  group_by(year, treat, index) %>% 
  summarise(value = mean(value))

df_edge2 = df_edge #To not have problem in diff...I dont know why!
colnames(df_edge2) = c("Ano", "Tratamento", "Indice", "Valor")

df_core = df %>% 
  filter(dist2 == "Interior") %>% 
  group_by(year, treat, index) %>% 
  summarise(value = mean(value))

df_core2 = df_core #To not have problem in diff...I dont know why!
colnames(df_core2) = c("Ano", "Tratamento", "Indice", "Valor")


ggplot(df_edge2, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group=Tratamento), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Tratamento))+
  facet_grid(rows = vars(Indice), scales = "free")+
  theme_bw()+
  ggtitle("Edge")+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))

ggplot(df_core2, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group=Tratamento), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Tratamento))+
  facet_grid(rows = vars(Indice), scales = "free")+
  theme_bw()+
  ggtitle("Core")+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))


#Difference from Control ===========================================
#Calculate difference in relation of control
df_crt = filter(df_edge, treat == "Controle")
df_b3yr = filter(df_edge, treat == "B3yr")
df_b1yr = filter(df_edge, treat == "B1yr")

df_b3yr$value = 100 - ((df_b3yr$value*100)/df_crt$value)
df_b1yr$value = 100 - ((df_b1yr$value*100)/df_crt$value)
df_diff = rbind(df_b3yr, df_b1yr)
colnames(df_diff) = c("Ano", "Tratamento", "Indice","Valor")
df_dif_ed = df_diff
df_dif_ed$Valor = abs(df_dif_ed$Valor)

df_crt = filter(df_core, treat == "Controle")
df_b3yr = filter(df_core, treat == "B3yr")
df_b1yr = filter(df_core, treat == "B1yr")

df_b3yr$value = 100 - ((df_b3yr$value*100)/df_crt$value)
df_b1yr$value = 100 - ((df_b1yr$value*100)/df_crt$value)
df_diff = rbind(df_b3yr, df_b1yr)
colnames(df_diff) = c("Ano", "Tratamento", "Indice","Valor")
df_dif_co = df_diff
df_dif_co$Valor = abs(df_dif_co$Valor)

a = ggplot(df_dif_ed, aes(x=Ano, y=Valor, color = Indice))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Indice), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_grid(rows = vars(Tratamento))+
  labs(y = NULL)+
  theme_bw()+
  ggtitle("Edge")+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  scale_color_manual(values = c('#377eb8','#1b9e77','#e41a1c','darkred','#ff7f00','#4daf4a'))+
  theme(text = element_text(family = "Times New Roman", size = 14))

a

b = ggplot(df_dif_co, aes(x=Ano, y=Valor, color = Indice))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Indice), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_grid(rows = vars(Tratamento))+
  labs(y = NULL)+
  theme_bw()+
  ggtitle("Core")+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  scale_color_manual(values = c('#377eb8','#1b9e77','#e41a1c','darkred','#ff7f00','#4daf4a'))+
  theme(text = element_text(family = "Times New Roman", size = 14))

b

#ggsave(filename = "Diff_Edge.png", plot = a,
 #    path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Landsat Time Series", width = 20, height = 10, units = "cm", dpi = 300)

#ggsave(filename = "Diff_Core.png", plot = b,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Landsat Time Series", width = 20, height = 10, units = "cm", dpi = 300)





















