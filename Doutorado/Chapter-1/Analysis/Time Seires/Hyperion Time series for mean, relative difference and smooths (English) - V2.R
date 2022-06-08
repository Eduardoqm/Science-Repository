#########################################################
# Hyperion Time series for mean and relative difference #
#                                                       #
# Eduardo Q Marques 29-09-2021                          #
#########################################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(mgcv)
library(visreg)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)
windowsFonts("Times New Roman" = windowsFont("Times New Roman"))

#Data ============================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

#Modify Data =====================================================
df$year = as.numeric(df$year)
df$index = as.character(df$index)
df$treat = as.character(df$treat)

#Change names format
df$index[df$index == "evi2"] <- c("EVI")
df$index[df$index == "ndvi"] <- c("NDVI")
df$index[df$index == "ndii"] <- c("NDII")
df$index[df$index == "vig"] <- c("VIG")
df$index[df$index == "vari"] <- c("VARI")
df$index[df$index == "nirv"] <- c("NIRv")
df$index[df$index == "lwvi2"] <- c("LWVI2")
df$index[df$index == "msi"] <- c("MSI")
df$index[df$index == "ndwi"] <- c("NDWI")
df$index[df$index == "pssr"] <- c("PSSR")
df$index[df$index == "psri"] <- c("PSRI")
df$index[df$index == "sipi"] <- c("SIPI")
df$index[df$index == "wbi"] <- c("WBI")
df$index[df$index == "pri"] <- c("PRI")
df$index[df$index == "rendvi"] <- c("RENDVI")
df$index[df$index == "nbr"] <- c("NBR")
df$index[df$index == "nbr2"] <- c("NBR2")

df$treat[df$treat == "control"] <- c("Control")
df$treat[df$treat == "b3yr"] <- c("B3yr")
df$treat[df$treat == "b1yr"] <- c("B1yr")

eqm = c("orange", "red", "blue") #My color palette

#Mean time series =================================================
df_m = df %>% 
  na.omit() %>% 
  group_by(year, treat, index) %>% 
  summarise(value = mean(value))

#df_m$value[df_m$index == "NDWI"] <- (df_m$value)/10
df_m2 = df_m #To not have problem in diff...I dont know why!
colnames(df_m2) = c("Ano", "Treatment", "Indice", "Valor")

ggplot(df_m2, aes(x=Ano, y=Valor, color = Treatment))+
  geom_line(aes(group=Treatment), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Treatment))+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  facet_wrap(vars(Indice), scales = "free")+
  xlab(NULL)+ylab(NULL)+
  theme_bw()+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14),
        legend.position = c(0.5, 0.1))

#Diference by sbtraction ===========================================
#Calculate difference in relation of control
df_crt = filter(df_m, treat == "Control")
df_b3yr = filter(df_m, treat == "B3yr")
df_b1yr = filter(df_m, treat == "B1yr")

df_b3yr$value = 100 - ((df_b3yr$value*100)/df_crt$value)
df_b1yr$value = 100 - ((df_b1yr$value*100)/df_crt$value)
df_diff = rbind(df_b3yr, df_b1yr)
colnames(df_diff) = c("Ano", "Treatment", "Indice", "Valor")

#df_diff$Valor = abs(df_diff$Valor) #Turn percentege to positive
df_diff2 = df_diff
df_diff2$Indice <- factor(df_diff2$Indice,      # Reordering group factor levels
                        levels = c("PSRI","VIG","VARI","MSI","PSSR","LWVI2","NDII","NBR2","NBR","NIRv","NDWI","EVI","NDVI","PRI","RENDVI","SIPI","WBI"))  
                       #levels = c("EVI","NDVI","VARI","VIG",
                        #          "LWVI2","MSI","NDII","NDWI","NIRv","PSRI","PSSR","SIPI","WBI",
                         #         "PRI","RENDVI","NBR","NBR2"))





all_vis = ggplot(df_diff2, aes(x=Ano, y=Valor, color = Treatment))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Treatment), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_wrap(vars(Indice), scales = "free", ncol = 5)+
  labs(x = NULL, y = NULL, title =  NULL)+
  scale_color_manual(values=c( "orange", "red"))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  theme(legend.position = c(0.5, 0.1)); all_vis


#ggsave(filename = "all_diff.png", plot = all_vis,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Hyperion Time Series",
#      width = 30, height = 20, units =  "cm", dpi = 300)

struc = df_diff %>% 
  filter(Indice %in% c("EVI","NDVI","VARI","VIG"))

a = ggplot(struc, aes(x=Ano, y=Valor, color = Treatment))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Treatment), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_wrap(vars(Indice), scales = "free", ncol = 5)+
  labs(x = NULL, y = NULL, title =  "Structural")+
  scale_color_manual(values=c( "orange", "red"))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14)); a


bioc1 = df_diff %>% 
  filter(Indice %in% c("LWVI2","MSI","NDII","NDWI","NIRv"))

b1 = ggplot(bioc1, aes(x=Ano, y=Valor, color = Treatment))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Treatment), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_wrap(vars(Indice), scales = "free", ncol = 5)+
  labs(x = NULL, y = NULL, title =  "Biochemical")+
  scale_color_manual(values=c( "orange", "red"))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  theme(legend.position = "none"); b1

bioc2 = df_diff %>% 
  filter(Indice %in% c("PSRI","PSSR","SIPI","WBI"))

b2 = ggplot(bioc2, aes(x=Ano, y=Valor, color = Treatment))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Treatment), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_wrap(vars(Indice), scales = "free", ncol = 5)+
  labs(x = NULL, y = NULL, title =  NULL)+
  scale_color_manual(values=c( "orange", "red"))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14)); b2

phy_fire = df_diff2 %>% 
  filter(Indice %in% c("PRI","RENDVI","NBR","NBR2"))

c = ggplot(phy_fire, aes(x=Ano, y=Valor, color = Treatment))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Treatment), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_wrap(vars(Indice), scales = "free", ncol = 5)+
  labs(x = NULL, y = NULL, title =  "Physiological and Fire")+
  scale_color_manual(values=c( "orange", "red"))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14)); c




ggarrange(a, b1, b2, c, nrow = 4)














