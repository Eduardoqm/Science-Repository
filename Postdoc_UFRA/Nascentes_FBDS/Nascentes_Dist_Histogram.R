#Histogram of distance

#Eduardo Q Marques 16-09-2024

library(tidyverse)
library(moments)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Parados/Nascentes_FBDS (Silverio et al)/Modelos_nascentes")
dir()

df=readxl::read_excel("Nascentes_riacho_fundo_barra.xlsx")
head(df)

#Separate Negatives and positives values ---------------------------------------
dfn = df %>%
  filter(dif_elev < 0)

#dfn$mindist_m = 0 - dfn$mindist_m

dfp = df %>%
  filter(dif_elev >= 0)

dfp$mindist_m = 0 - dfp$mindist_m

df2 = rbind(dfp, dfn)

#Calculating percentage ranges -------------------------------------------------
#General
quantile(df$mindist_m, probs = 0.9) #909.43
quantile(df$mindist_m, probs = 0.1) #573.69
quantile(df$mindist_m, probs = 0.7655) #495.58

#Positive
quantile(dfp$mindist_m, probs = 0.9) #831.15
quantile(dfp$mindist_m, probs = 0.8) #416.98
quantile(dfp$mindist_m, probs = 0.7) #229.87

#Negative
quantile(dfn$mindist_m, probs = 0.9) #-35.57
quantile(dfn$mindist_m, probs = 0.8) #-52.05
quantile(dfn$mindist_m, probs = 0.7) #-107.72






#Graphics ----------------------------------------------------------------------
#General
ggplot(df, aes(x = mindist_m))+
  geom_density(fill = "black", alpha = 0.30)+
  geom_vline(xintercept = 0,linetype = "dashed", col = "black", size = 1)+

  geom_vline(xintercept = 385.67,linetype = "dashed", col = "black", size = 1)+
  geom_vline(xintercept = 573.69,linetype = "dashed", col = "black", size = 1)+
  geom_vline(xintercept = 909.43,linetype = "dashed", col = "black", size = 1)+

  annotate(geom = "text", x = 192.83, y = 0.0010, col = "black", label = "385.67 m
  (70%)")+
  annotate(geom = "text", x = 470, y = 0.0010, col = "black", label = "573.69 m
  (80%)")+
  annotate(geom = "text", x = 750, y = 0.0010, col = "black", label = "909.43 m
  (90%)")+

  labs(x = "Distance difference (m)", y = NULL)+
  theme_minimal()

#Positive and negative
df500 = df %>% filter(mindist_m <= 500)


quantile(df$mindist_m, probs = 0.7655) #495.58 Until 500

quantile(df2$mindist_m, probs = 0.0445) #-497.52 less than -500
quantile(df2$mindist_m, probs = 0.8145) #499.86 higher than 500


plot1 = ggplot(df2, aes(x = mindist_m))+
  geom_density(fill = "royalblue", alpha = 0.30)+

  geom_vline(xintercept = 0,linetype = "dashed", col = "darkgray", size = 1)+
  annotate(geom = "text", x = -800, y = 0.00075, col = "darkgray", fontface = 2,
           label = "27.27%
  less than zero
  
  4.45%
  less than -500")+
  
  annotate(geom = "text", x = 800, y = 0.00075, col = "darkgray", fontface = 2,
           label = "72.72%
  higher than zero
           
  19%
  higher than 500")+

  geom_vline(xintercept = -500,linetype = "dashed", col = "black", size = 1)+
  geom_vline(xintercept = 500,linetype = "dashed", col = "black", size = 1)+
  annotate(geom = "text", x = 0, y = 0.0001, col = "darkblue", fontface = 2,
           label = "76.55%
  with 500 m of difference")+
  

  labs(x = "Distance difference (m)", y = NULL)+
  theme_minimal(); plot1

ggsave(filename = "Histogram_distance.tiff", plot = plot1,
       width = 17, height = 10, units = "cm", dpi = 300)


skewness(df2$mindist_m) #-0.06800383
kurtosis(df2$mindist_m) #5.130579





#Distancia entre as nascentes observadas e preditas é menor que 1,5 km,
#para 80% dos casos. em distancia de 1,0 km temos 70%.

#se a curtose for negativa indica uma subestimativa das areas de APP,
#ou seja a area de app real é maior que o calculado utilizando a localizacao
#das nascentes. Se for positiva, a area de app seria maior que o realmente é....


