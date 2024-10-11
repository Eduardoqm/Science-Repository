#Spectral response from Landsat8 and Sentinel2

#Eduardo Q Marques 05-02-2024

library(tidyverse)
library(terra)
library(svglite)

#Load Data ---------------------------------------------------------------------
setwd("C:/Users/Workshop/Documents/Research/Postdoc_UFRA/Papers/Culturas_permanentes")
dir()

#Polygons of permanent production ----------------------------------------------
cult = vect("cultures_forest_pasture_guama.shp")
#plot(cult)

cultdf = as.data.frame(cult)

#Landsat-8 reflectance =========================================================
l8 = read.csv("Spectral_signture/Landsat_8b.csv")

colnames(l8)[2:8] = c("Ultra_Blue", "Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")

l8[,c(2:8)] = (l8[,c(2:8)]*0.0000275)+(-0.2) #Converting for reflectance


#Join and process data ---------------------------------------------------------
df = full_join(cultdf, l8, by = "id")

#Change names of productions
df$cultura[df$cultura == "citros"] = c("Citrus")
df$cultura[df$cultura == "dende"] = c("Dende")
df$cultura[df$cultura == "pimenta"] = c("Pepper")

#Spectral Reflectance curve ----------------------------------------------------
dfland = df %>% 
  select(cultura, Blue, Green, Red, NIR, SWIR1, SWIR2) %>% 
  gather(band, value, -cultura)

dfland$band2 = dfland$band

#dfland$band2[dfland$band2 == "Ultra_Blue"] = c(430)
dfland$band2[dfland$band2 == "Blue"] = c(450)
dfland$band2[dfland$band2 == "Green"] = c(530)
dfland$band2[dfland$band2 == "Red"] = c(640)
dfland$band2[dfland$band2 == "NIR"] = c(850)
dfland$band2[dfland$band2 == "SWIR1"] = c(1570)
dfland$band2[dfland$band2 == "SWIR2"] = c(2110)

dfland$band2 = as.numeric(dfland$band2)
dfland$satt = c("Landsat-8")




#Sentinel-2 reflectance ========================================================
s2 = read.csv("Spectral_signture/Sentinel_2B.csv")

colnames(s2)[2:12] = c("Ultra_Blue", "Blue", "Green", "Red",
                       "RedEdge1", "RedEdge2", "RedEdge3",
                       "NIR", "NIRn", "SWIR1", "SWIR2")

s2 = s2[-5293,] #Remove an outlayer line

#Join and process data ---------------------------------------------------------
df = full_join(cultdf, s2, by = "id")

#Change names of productions
df$cultura[df$cultura == "citros"] = c("Citrus")
df$cultura[df$cultura == "dende"] = c("Dende")
df$cultura[df$cultura == "pimenta"] = c("Pepper")

#Spectral Reflectance curve ----------------------------------------------------
dfsent = df %>% 
  select(cultura, Blue, Green, Red,
         RedEdge1, RedEdge2, RedEdge3,
         NIR, NIRn, SWIR1, SWIR2) %>% 
  gather(band, value, -cultura)

dfsent$band2 = dfsent$band

#dfsent$band2[dfsent$band2 == "Ultra_Blue"] = c(433)
dfsent$band2[dfsent$band2 == "Blue"] = c(458)
dfsent$band2[dfsent$band2 == "Green"] = c(543)
dfsent$band2[dfsent$band2 == "Red"] = c(650)
dfsent$band2[dfsent$band2 == "RedEdge1"] = c(698)
dfsent$band2[dfsent$band2 == "RedEdge2"] = c(733)
dfsent$band2[dfsent$band2 == "RedEdge3"] = c(773)
dfsent$band2[dfsent$band2 == "NIR"] = c(785)
dfsent$band2[dfsent$band2 == "NIRn"] = c(855)
dfsent$band2[dfsent$band2 == "SWIR1"] = c(1565)
dfsent$band2[dfsent$band2 == "SWIR2"] = c(2100)

dfsent$band2 = as.numeric(dfsent$band2)
dfsent$satt = c("Sentinel-2")



#Plot joint ====================================================================
dfsat = rbind(dfland, dfsent)


#curv = ggplot(dfsat, aes(x=band2, y=value, col = cultura))+
#  stat_summary(geom = "point", fun.y = "mean", size = 3, alpha = 0.7)+
#  stat_summary(geom = "line", fun.y = "mean", size = 1, alpha = 0.7)+
#  geom_vline(xintercept=c(458, 698, 785, 1565), linetype="dashed", color = "darkgray")+
  #annotate("text", x = c(433, 615, 785, 1565), y = c(0.4, 0.05, 0.4, 0.4),
   #        col = "#969696", hjust = -0.3,
    #       label= c("Visible", "RedEdge", "Near InfraRed", "Mid InfraRed"))+
#  scale_color_manual(values = c("#9932cc", "#9065d0", "#32a65e", "#edde8e", "#e6ccff"),
#                     name = "Culture")+
#  facet_wrap(~satt, ncol = 1)+
#  labs(x="Wavelenght (nm)", y = "Mean Reflectance (%)",
#       title = "Spectral Reflectance Curve")+
#  theme_bw(); curv

dfsat2 = dfsat %>% 
  na.omit() %>% 
  group_by(cultura, band, band2, satt) %>% 
  summarise(mean = mean(value),
            low = quantile(value, 0.25),
            upper = quantile(value, 0.75))

curv = ggplot(dfsat2, aes(x=band2, y=mean, col = cultura))+
  geom_point(size = 3, alpha = 0.7)+
  geom_line(size = 1, alpha = 0.7)+
  geom_errorbar(aes(ymin = low, ymax = upper), width = 50)+
  geom_vline(xintercept=c(458, 698, 785, 1565), linetype="dashed", color = "darkgray")+
  scale_color_manual(values = c("#9932cc", "#9065d0", "#32a65e", "#edde8e", "#e6ccff"),
                   name = "Culture")+
  facet_wrap(~satt, ncol = 1)+
  labs(x="Wavelenght (nm)", y = "Mean Reflectance (%)",
       title = "Spectral Reflectance Curve")+
  theme_bw(); curv


ggsave(plot = curv, filename = "Spect_Curve_Satellites.svg",
       path = "C:/Users/Workshop/Documents/Research/Postdoc_UFRA/Papers/Culturas_permanentes/Figuras",
       height = 15, width = 15, units = "cm", dpi = 300)

ggsave(plot = curv, filename = "Spect_Curve_Satellites.png",
       path = "C:/Users/Workshop/Documents/Research/Postdoc_UFRA/Papers/Culturas_permanentes/Figuras",
       height = 15, width = 15, units = "cm", dpi = 300)
