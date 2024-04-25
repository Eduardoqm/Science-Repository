#Extreme Wind Gust Recurrence vs (DEM, Precipitation, Vegetation Height) - V2

#Bonus: Map of DEM

#Eduardo Q Marques 03-11-2023

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(scales)
library(viridis)
library(terra)
library(sf)

#Load data --------------------------------------------------------------------
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")

dem = read.csv("DEM_Xingu.csv")

wg = read.csv("Extreme_WG_Recurrence.csv")

gedi = read.csv("GEDI_mean_pixel.csv", sep = ",")
colnames(gedi) = c("id", "Higth", "x", "y")

bioma = read.csv("Biome_coord.csv", sep = ",")
colnames(bioma) = c("x", "y", "Biome")

prec = read.csv("Accumulated_Precipitation_ERA5L.csv", sep = ",")

opt_chi = read.csv("Optimal_ERA5_Block_1979_2020.csv", sep = ",")

chi = opt_chi %>%
  group_by(x, y) %>%
  slice(which.max(CHI))
chi = chi[,c(5,6,7)]

#Join data frames -------------------------------------------------------------
dem = dem %>% unite("xy", x:y)
wg = wg %>% unite("xy", x:y)
prec = prec %>% unite("xy", x:y)
gedi = gedi %>% unite("xy", x:y)
bioma = bioma %>% unite("xy", x:y)
chi = chi %>% unite("xy", x:y)

df = full_join(dem, wg, by = "xy")
df = full_join(df, prec, by = "xy")
df = full_join(df, gedi, by = "xy")
df = full_join(df, chi, by = "xy")
df = full_join(df, bioma, by = "xy")


df = df %>% na.omit()
df = df[,c(-5)]

df$Frequency = df$Recurrence/41

pont = df %>% filter(xy == "-52.25_-13") #Area1 point

#Regression with quadratic factor for Wind Frequency x DEM -------------------
model = lm(Frequency~DEM+I(DEM^2), data = df)
summary(model)

df$predito=predict(model)

model2 = lm(Frequency~Precipitation+I(Precipitation^2), data = df)
summary(model2)

df$predito2=predict(model2)

#Correlation plots ------------------------------------------------------------
a = ggplot(df, aes(x=CHI, y=Frequency, col = Biome))+
  geom_point(size = 4, alpha = 0.7)+
  geom_point(data = pont, aes(x=CHI, y=Frequency), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  stat_cor(show.legend = F)+
  geom_smooth(col = "black", method = "lm")+
  scale_colour_manual(values = c("darkgreen", "darkorange"))+
  labs(x="CHI Coefficient",
       y="Frequency/year of Wind Gust (>10 m/s)",
       title = "a) Extreme Wind (1979-2020) x Opitmal CHI")+
  #theme_minimal()+
  theme(legend.position = c(0.85, 0.8)); a

a2 = ggplot(df, aes(x=CHI, y=Precipitation, col = Biome))+
  geom_point(size = 4, alpha = 0.7)+
  geom_point(data = pont, aes(x=CHI, y=Precipitation), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  stat_cor(show.legend = F)+
  geom_smooth(col = "black", method = "lm")+
  scale_colour_manual(values = c("darkgreen", "darkorange"))+
  labs(x="CHI Coefficient",
       y="Accumulated Precipitation (mm)",
       title = "a) Annual Accumulated Precipitation (1979-2020) x Opitmal CHI")+
  #theme_minimal()+
  theme(legend.position = c(0.85, 0.8)); a2

a3 = ggplot(df, aes(x=Precipitation, y=Frequency))+
  geom_point(aes(col = Biome), size = 4, alpha = 0.7)+
  geom_point(data = pont, aes(x=Precipitation, y=Frequency),
             col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  #stat_cor(show.legend = F)+
  stat_cor(show.legend = F, label.y.npc="top", label.x.npc = 0.7)+
  stat_cor(aes(col = Biome), show.legend = F, label.y.npc = 0.85, label.x.npc = 0.7)+
  #geom_smooth(col = "black", method = "lm")+
  geom_smooth(aes(x=Precipitation, y=predito2), col = "black")+
  scale_colour_manual(values = c("darkgreen", "darkorange"))+
  labs(x="Accumulated Precipitation (mm)",
       y="Frequency/year of Wind Gust (>10 m/s)",
       title = "Accumulated Precipitation x Extreme Wind (1979-2020)"); a3
  #theme_minimal()+
  #theme(legend.position = c(0.85, 0.8)); a3


b = ggplot(df, aes(x=DEM, y=Frequency))+
  geom_point(aes(col = Biome), size = 4, alpha = 0.7)+
  geom_point(data = pont, aes(x=DEM, y=Frequency), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  stat_cor(show.legend = F)+
  geom_smooth(aes(x=DEM, y=predito), col = "black")+
  scale_colour_manual(values = c("darkgreen", "darkorange"))+
  labs(x="Elevation Above Sea Level (m)",
       y="Frequency/year of Wind Gust (>10 m/s)",
       title = "c) Extreme Wind (1979-2020) x Elevation Model")+
  #theme_minimal()+
  theme(legend.position = c(0.85, 0.2)); b

b2 = ggplot(df, aes(x=DEM, y=Frequency))+
  geom_point(aes(size = Precipitation, col = Biome), alpha = 0.7)+
  geom_point(data = pont, aes(x=DEM, y=Frequency), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  #ylim(4, 25)+
  #stat_cor(show.legend = F)+
  annotate("text", x = 275, y = 20, label = "R2 = 0.67, p < 0.005")+
  geom_smooth(aes(x=DEM, y=predito), col = "black")+
  scale_colour_manual(values = c("darkgreen", "darkorange"))+
  labs(x="Elevation Above Sea Level (m)",
       y="Frequency/year of Wind Gust (>10 m/s)",
       title = "b)"); b2
  #theme_minimal()+
  #theme(legend.position = c(0.91, 0.45)); b2
#Multiple R-squared:  0.6754,	Adjusted R-squared:  0.6734 
#F-statistic: 346.4 on 2 and 333 DF,  p-value: < 2.2e-16


c = ggplot(df, aes(x=Frequency, y=Higth))+
  geom_point(aes(col = Biome), size = 4, alpha = 0.7)+
  geom_point(data = pont, aes(x=Frequency, y=Higth), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  stat_cor(show.legend = F, label.y.npc="top", label.x.npc = 0.7)+
  stat_cor(aes(col = Biome), show.legend = F, label.y.npc = 0.85, label.x.npc = 0.7)+
  #facet_wrap(~Biome)+
  scale_colour_manual(values = c("darkgreen", "darkorange"))+
  geom_smooth(method = "lm", col = "black")+
  labs(x="Frequency/year of Wind Gust (>10 m/s)",
       y="Vegetation Height (m)",
       title = "e) Forest Height (2019) x Extreme Wind (1979-2020)")+
  #theme_minimal()+
  theme(legend.position = c(0.09, 0.15)); c



# Linear Regression for Wind Frequency x Vegetation Height ---------------------
struct = lm(Higth~Frequency, data = df)
summary(struct)
df$preditoveg=predict(struct)
#Multiple R-squared:  0.1989,	Adjusted R-squared:  0.1965 
#F-statistic: 82.91 on 1 and 334 DF,  p-value: < 2.2e-16

df_am = df %>% filter(Biome == "Amazonia")
struct_am = lm(Higth~Frequency, data = df_am)
summary(struct_am)
df_am$preditoam=predict(struct_am)
#Multiple R-squared:  0.0934,	Adjusted R-squared:  0.08938 
#F-statistic: 23.28 on 1 and 226 DF,  p-value: 2.574e-06

df_ce = df %>% filter(Biome == "Cerrado")
struct_ce = lm(Higth~Frequency, data = df_ce)
summary(struct_ce)
df_ce$preditoce=predict(struct_ce)
#Multiple R-squared:  0.03324,	Adjusted R-squared:  0.02412 
#F-statistic: 3.644 on 1 and 106 DF,  p-value: 0.05896

c2 = ggplot(df, aes(x=Frequency, y=Higth))+
  geom_point(aes(size = Precipitation, col = Biome), alpha = 0.7)+
  geom_point(data = pont, aes(x=Frequency, y=Higth), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  #stat_cor(show.legend = F, label.y.npc="top", label.x.npc = 0.7)+
  #stat_cor(aes(col = Biome), show.legend = F, label.y.npc = 0.85, label.x.npc = 0.7)+
  #facet_wrap(~Biome)+
  scale_colour_manual(values = c("darkgreen", "darkorange"))+
  #geom_smooth(method = "lm", col = "black")+
  geom_smooth(aes(x=Frequency, y=preditoveg), col = "black")+
  #geom_smooth(data = df_am, aes(x=Frequency, y=preditoam), col = "darkgreen")+
  #geom_smooth(data = df_ce, aes(x=Frequency, y=preditoce), col = "darkorange")+
  annotate("text", x = 17.5, y = 22.5, label = "R2 = 0.20, p < 0.005")+
  annotate("text", x = 17.5, y = 20, label = "R2 = 0.09, p < 0.005", col = ("darkgreen"))+
  annotate("text", x = 17.35, y = 17.5, label = "R2 = 0.03, p = 0.06", col = ("darkorange"))+
  labs(x="Frequency/year of Wind Gust (>10 m/s)",
       y="Vegetation Height (m)",
       title = "c)"); c2
  #theme_minimal()+
  #theme(legend.position = c(0.09, 0.15)); c2


ggsave(filename = "Extreme Wind (1979-2020) x CHI - B.png", plot = a3,
       path = "C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 16, height = 10, units = "cm", dpi = 300)

ggsave(filename = "Extreme Wind (1979-2020) x Elevation Model - B.tiff", plot = b2,
       path = "C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 16, height = 10, units = "cm", dpi = 300)

ggsave(filename = "Forest Height x Extreme Wind (1979-2020) - B.tiff", plot = c2,
       path = "C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 16, height = 10, units = "cm", dpi = 300)






