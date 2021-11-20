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
df_smt = df #%>% select(x, y, index, year, treat, value)
#df_smt$year = as.character(df_smt$year)
colnames(df_smt) = c("x","y","Indice","Ano","Tratamento","Valor","Dist","Dist2")

eqm = c("orange", "red", "blue") #My color palette

smtplot = ggplot(df_smt, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.5, size = 1, level = 0.9999999999999)+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
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
df_m = df2 %>% 
  group_by(year, treat, index) %>% 
  summarise(value = mean(value))

df_m2 = df_m #To not have problem in diff...I dont know why!
colnames(df_m2) = c("Ano", "Tratamento", "Indice", "Valor")

ggplot(df_m2, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            fill = "black", color = NA, alpha = 0.002)+
  geom_line(aes(group=Tratamento), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Tratamento))+
  #geom_vline(xintercept = "2004", linetype = "dashed")+
  #geom_vline(xintercept = "2011", linetype = "dashed")+
  #stat_summary(geom="line", fun.y="mean", size = 1.5, aes(group=Tratamento))+
  #stat_summary(geom="point", fun.y="mean", size = 2, aes(group=Tratamento))+
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
colnames(df_diff) = c("Ano", "Tratamento", "Indice", "Valor")
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
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Landsat Time Series", width = 20, height = 10, units =  "cm", dpi = 300)



#Test ZONE =============================================================
ggplot(df_diff, aes(x=Ano, y=Valor, color = Indice))+
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


#Apresentations tests ==================================================
library(gganimate)

ggplot(df_diff, aes(x=Ano, y=Valor, color = Indice))+
  geom_line(aes(group = Indice), size = 1.5, alpha = 0.95)+
  geom_text(x=2007.5, y=85, label="Período
  do fogo", col = "red", fontface="bold")+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "red")+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "red")+
  facet_grid(rows = vars(Tratamento))+
  labs(y = "Diferença em relação o Controle (%)")+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = c('#377eb8','#1b9e77','#e41a1c','darkred','#ff7f00','#4daf4a'))+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  transition_reveal(Ano)

#anim_save("Landsat_relative_diff.gif")

library(plotly)

gg = ggplot(df_diff, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            fill = "black", color = NA, alpha = 0.002)+
  geom_line(aes(group = Tratamento), size = 1.5, alpha = 0.7)+
  geom_point()+
  facet_grid(rows = vars(Indice), scales = "free")+
  labs(y = "Diferença em relação o Controle (%)")+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))

ggplotly(difplot)

#Test with ggarrange
library(ggpubr)

a = ggplot(df_smt, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.5, size = 1.2, level = 0.9999999999999)+
  geom_vline(xintercept = "2004", linetype = "dashed")+
  geom_vline(xintercept = "2011", linetype = "dashed")+
  #stat_summary(geom="line", fun.y="mean", size = 0.5, linetype = "dashed", aes(group=Tratamento))+
  #stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Tratamento))+
  facet_grid(rows = vars(Indice), scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))

b = ggplot(df_m2, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            fill = "black", color = NA, alpha = 0.002)+
  geom_line(aes(group=Tratamento), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Tratamento))+
  #geom_vline(xintercept = "2004", linetype = "dashed")+
  #geom_vline(xintercept = "2011", linetype = "dashed")+
  #stat_summary(geom="line", fun.y="mean", size = 1.5, aes(group=Tratamento))+
  #stat_summary(geom="point", fun.y="mean", size = 2, aes(group=Tratamento))+
  facet_grid(rows = vars(Indice), scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))

ggarrange(b+rremove("xlab")+rremove("ylab"),
                a+rremove("xlab")+rremove("ylab"),
                common.legend = TRUE,
                legend="right",
                ncol = 2, nrow = 1)
