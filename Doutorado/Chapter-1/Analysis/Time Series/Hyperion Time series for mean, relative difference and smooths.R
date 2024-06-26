#########################################################
# Hyperion Time series for mean and relative difference #
#                                                       #
# Eduardo Q Marques 29-09-2021                          #
#########################################################

library(tidyverse)
library(reshape2)
library(ggplot2)
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

df$treat[df$treat == "control"] <- c("Controle")
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
colnames(df_m2) = c("Ano", "Tratamento", "Indice", "Valor")

ggplot(df_m2, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_line(aes(group=Tratamento), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Tratamento))+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  facet_wrap(vars(Indice), scales = "free")+
  xlab(NULL)+ylab(NULL)+
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
colnames(df_diff) = c("Ano", "Tratamento", "Indice", "Valor")

#df_diff$Valor = abs(df_diff$Valor) #Turn percentege to positive

all_vis = ggplot(df_diff, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Tratamento), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_wrap(vars(Indice), scales = "free")+
  xlab(NULL)+ylab(NULL)+
  scale_color_manual(values=c( "orange", "red"))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  theme(legend.position = c(0.5, 0.1))
all_vis

#ggsave(filename = "all_diff.png", plot = all_vis,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Hyperion Time Series",
#      width = 30, height = 20, units =  "cm", dpi = 300)

#Plot by kind of Indice ---------------------------------------------------
struc = df_diff %>% 
  filter(Indice %in% c("EVI","NDVI","VARI","VIG"))

a = ggplot(struc, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Tratamento), size = 1.5, alpha = 0.8)+
  geom_point(size = 2, alpha = 0.8)+
  facet_wrap(vars(Indice), scales = "free")+
  xlab(NULL)+ylab("Diferen�a em rela��o o Controle (%)")+
  scale_color_manual(values=c( "orange", "red"))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14))

#ggsave(filename = "Structural_diff.png", plot = a,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Hyperion Time Series", width = 30, height = 15, units =  "cm", dpi = 300)


bioc = df_diff %>% 
  filter(Indice %in% c("LWVI2","MSI","NDII","NDWI","NIRv","PSRI","PSSR","SIPI","WBI"))

b = ggplot(bioc, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Tratamento), size = 1.5, alpha = 0.8)+
  geom_point(size = 2, alpha = 0.8)+
  facet_wrap(vars(Indice), scales = "free")+
  xlab(NULL)+ylab("Diferen�a em rela��o o Controle (%)")+
  scale_color_manual(values=c( "orange", "red"))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14))

#ggsave(filename = "Biochemistry_diff.png", plot = b,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Hyperion Time Series", width = 30, height = 15, units =  "cm", dpi = 300)

phy_fire = df_diff %>% 
  filter(Indice %in% c("NBR","NBR2","PRI","RENDVI"))

c = ggplot(phy_fire, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Tratamento), size = 1.5, alpha = 0.8)+
  geom_point(size = 2, alpha = 0.8)+
  facet_wrap(vars(Indice), scales = "free")+
  xlab(NULL)+ylab("Diferen�a em rela��o o Controle (%)")+
  scale_color_manual(values=c( "orange", "red"))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14))


#ggsave(filename = "Physiologic_Fire_diff.png", plot = c,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Hyperion Time Series", width = 30, height = 15, units =  "cm", dpi = 300)

#Biggest differences
top_vis = df_diff %>% 
  filter(Indice %in% c("PSRI","VIG","VARI","MSI","PSSR","NDII"))

topvis = ggplot(top_vis, aes(x=Ano, y=Valor, color = Tratamento))+
  #geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  #geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Tratamento), size = 1.5, alpha = 0.8)+
  geom_point(size = 2, alpha = 0.8)+
  facet_wrap(vars(Indice), scales = "free")+
  xlab(NULL)+ylab("Diferen�a em rela��o o Controle (%)")+
  scale_color_manual(values=c("orange", "red"))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14))

#ggsave(filename = "Top_diff.png", plot = topvis,
 #     path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Hyperion Time Series", width = 30, height = 15, units =  "cm", dpi = 300)

#Carotenoides VIs
pigm = df_diff %>% 
  filter(Indice %in% c("PSRI","PSSR"))

ggplot(pigm, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Tratamento), size = 1.5, alpha = 0.8)+
  geom_point(size = 2, alpha = 0.8)+
  facet_grid(rows = vars(Indice), scales = "free")+
  xlab(NULL)+ylab("Diferen�a em rela��o o Controle (%)")+
  scale_color_manual(values=c("orange", "red"))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14))

#Another model
ggplot(bioc, aes(x=Ano, y=Valor, color = Indice))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Indice), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_grid(rows = vars(Tratamento), scales = "free")+
  xlab(NULL)+ylab("Diferen�a em rela��o o Controle (%)")+
  #scale_color_manual(values=c('#ff7f00','darkgreen','#377eb8','darkred'))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14))



