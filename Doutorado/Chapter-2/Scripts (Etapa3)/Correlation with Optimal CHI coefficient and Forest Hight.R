#############################################################
# Correlation with Optimal CHI coefficient and Forest Hight #
#                                                           #
# Eduardo Q Marques 12-10-2022   updated: 23-10-2022        #
#############################################################

#rm(list =ls())
#.libPaths()
#install.packages("extRemes", type = "binary")

#.libPaths("C:/Users/queirozm/AppData/Local/Temp/Rtmpestoic/downloaded_packages")

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")

gedi = read.csv("GEDI_mean_pixel.csv", sep = ",")
colnames(gedi) = c("id", "Higth", "x", "y")

fire = read.csv("Fire_frequence_mean_pixel.csv", sep = ",")
colnames(fire) = c("id", "Fire", "x", "y")

bioma = read.csv("Biome_coord.csv", sep = ",")
colnames(bioma) = c("x", "y", "Biome")


opt_chi = read.csv("Optimal_ERA5_Block_1979_2020.csv", sep = ",")

opt_chi$Class = as.character(opt_chi$Class)
opt_chi$Class[opt_chi$Class == "1d, 0.25Â°"] = c("1d, 0.25°")
opt_chi$Class[opt_chi$Class == "1d, 0.75Â°"] = c("1d, 0.75°")
opt_chi$Class[opt_chi$Class == "1d, 1.25Â°"] = c("1d, 1.25°")
opt_chi$Class[opt_chi$Class == "1d, 1.75Â°"] = c("1d, 1.75°")

opt_chi$Class[opt_chi$Class == "3d, 0.25Â°"] = c("3d, 0.25°")
opt_chi$Class[opt_chi$Class == "3d, 0.75Â°"] = c("3d, 0.75°")
opt_chi$Class[opt_chi$Class == "3d, 1.25Â°"] = c("3d, 1.25°")
opt_chi$Class[opt_chi$Class == "3d, 1.75Â°"] = c("3d, 1.75°")

opt_chi$Class[opt_chi$Class == "5d, 0.25Â°"] = c("5d, 0.25°")
opt_chi$Class[opt_chi$Class == "5d, 0.75Â°"] = c("5d, 0.75°")
opt_chi$Class[opt_chi$Class == "5d, 1.25Â°"] = c("5d, 1.25°")
opt_chi$Class[opt_chi$Class == "5d, 1.75Â°"] = c("5d, 1.75°")

#Optimal Chi Coefficient by pixel ----------------------------------------------
opt_chi2 = opt_chi %>%
  group_by(x, y)%>%
  slice(which.max(CHI))%>%
  unite("xy", x:y)%>%
  ungroup()

gedi = gedi %>% unite("xy", x:y)

fire = fire %>% unite("xy", x:y)

bioma = bioma %>% unite("xy", x:y)

df = full_join(opt_chi2, gedi, id = "xy")
df = full_join(df, fire, id = "xy")
df = full_join(df, bioma, id = "xy")

df = df %>% na.omit()

#Area 1 point
pont = df %>% filter(xy == "-52.25_-13")

a = ggplot(df, aes(x=CHI, y=Higth))+
  geom_point(size = 3, col = "RoyalBlue", alpha = 0.7)+
  geom_point(data = pont, aes(x=CHI, y=Higth), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  stat_cor(show.legend = F)+
  labs( x = "CHI coefficient", y = "Vegetation Height (m)",
        title = "Forest Height (2019) x CHI (1979-2020)")+
  geom_smooth(col = "black")+
  theme_bw(); a


b = ggplot(df, aes(x=CHI, y=Higth, size=Fire))+
  geom_point(aes(col = Biome), alpha = 0.5)+
  geom_point(data = pont, aes(x=CHI, y=Higth), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  stat_cor(show.legend = F)+
  labs( x = "CHI coefficient", y = "Vegetation Height (m)",
        title = "Forest Height (2019) x CHI (1979-2020)")+
  geom_smooth(col = "black", method = "lm", se = T, show.legend = F)+
  scale_colour_manual(values = c("darkgreen", "darkorange"))+
  scale_size(range = c(1,5), breaks=c(2.5,8.5,17.5,26.25),labels=c(">2",">8",">17",">26"),guide="legend")+
  theme_bw(); b



ggsave(filename = "Xingu_OptCHI_Veg_mean_Height_1979_2020B.png", plot = b,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 14, height = 10, units = "cm", dpi = 300)


ggsave(filename = "Xingu_OptCHI_Veg_mean_Height_1979_2020.png", plot = a,
       path = "C:/Users/queirozm/Documents/Research/Capitulo2/Figuras/Paper_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)



c = ggplot(df, aes(x=CHI, y=Higth, size=Fire))+
  geom_point(aes(col = Biome), alpha = 0.5)+
  geom_point(data = pont, aes(x=CHI, y=Higth), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  stat_cor(show.legend = F)+
  labs( x = "CHI coefficient", y = "Vegetation Height (m)",
        title = "Forest Height (2019) x CHI (1979-2020)")+
  geom_smooth(col = "black", method = "lm", show.legend = F)+
  facet_wrap(~Biome)+
  scale_colour_manual(values = c("darkgreen", "darkorange"))+
  scale_size(range = c(1,5), breaks=c(2.5,8.5,17.5,26.25),labels=c(">2",">8",">17",">26"),guide="legend")+
  theme_minimal(); c

ggsave(filename = "Xingu_OptCHI_Veg_mean_Height_1979_2020C.png", plot = c,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 20, height = 10, units = "cm", dpi = 300)



model = lm(Higth~CHI+Fire, data = df)

summary(model)


d = ggplot(df, aes(y=CHI, x=Biome, fill = Biome))+
  geom_boxplot()+
  labs(x = NULL, y = "CHI coefficient",
       title = "Probability of extremes (1979-2020)")+
  scale_fill_manual(values = c("darkgreen", "darkorange"))+
  theme_bw(); d


ggsave(filename = "OptCHI_boxplot_Xingu_1979_2020.png", plot = d,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)





















