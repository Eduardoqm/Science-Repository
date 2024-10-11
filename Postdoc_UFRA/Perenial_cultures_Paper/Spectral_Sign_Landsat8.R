#Spectral response from Landsat8 

#Eduardo Q Marques 14-12-2023

library(tidyverse)
library(terra)

#Load Data ---------------------------------------------------------------------
setwd("C:/Users/Workshop/Documents/Research/Postdoc_UFRA/Papers/Culturas_permanentes (Silverio et al)")
dir()

#Polygons of permanent production ----------------------------------------------
cult = vect("cultures_forest_pasture_guama.shp")
#plot(cult)

cultdf = as.data.frame(cult)

#Landsat-8 reflectance
l8 = read.csv("Spectral_signture/Landsat_8b.csv")

colnames(l8)[2:8] = c("Ultra_Blue", "Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")

l8[,c(2:8)] = (l8[,c(2:8)]*0.0000275)+(-0.2) #Converting for reflectance


#Join and process data ---------------------------------------------------------
df = full_join(cultdf, l8, by = "id")

#Change names of productions
df$cultura[df$cultura == "citros"] = c("Citrus")
df$cultura[df$cultura == "dende"] = c("Dende")
df$cultura[df$cultura == "pimenta"] = c("Pepper")


df2 = df %>% 
  select(cultura, Blue, Green, Red, NIR, SWIR1, SWIR2) %>% 
  mutate(NDVI = ((NIR-Red)/(NIR+Red)),
         NDII = ((NIR-SWIR1)/(NIR+SWIR1)),
         EVI = 2.5*((NIR-Red)/(NIR+6*Red-7.5*Blue+1))) %>% 
  gather(band, value, -cultura)


#Plotting Results --------------------------------------------------------------
lplot = ggplot(df2, aes(x=cultura, y=value, fill=cultura))+
  geom_boxplot(alpha = 0.9)+
  #geom_jitter()+
  #geom_violin()+
  facet_wrap(~factor(band,
             levels = c("Blue", "Green", "Red",
                        "NIR", "SWIR1", "SWIR2",
                        "EVI", "NDVI", "NDII")),
             scale = "free")+
  scale_fill_manual(values = c("#9932cc", "#9065d0", "#32a65e", "#edde8e", "#e6ccff"))+
  #scale_fill_manual(values = c("orange", "darkgreen", "blue", "pink", "darkred"))+
  labs(x=NULL, y = NULL, title = "Spectral Signature from Landsat-8")+
  theme_bw()+
  theme(legend.position = c(100, 100))


#ggsave(plot = lplot, filename = "Spect_Sign_Landsat8.png",
#       path = "C:/Users/Workshop/Documents/Research/Postdoc_UFRA/Papers/Culturas_permanentes/Figuras",
#       height = 17, width = 25, units = "cm", dpi = 300)


#Spectral Reflectance curve ----------------------------------------------------
df3 = df %>% 
  select(cultura, Blue, Green, Red, NIR, SWIR1, SWIR2) %>% 
  gather(band, value, -cultura)

df3$band2 = df3$band

#df3$band2[df3$band2 == "Ultra_Blue"] = c(430)
df3$band2[df3$band2 == "Blue"] = c(450)
df3$band2[df3$band2 == "Green"] = c(530)
df3$band2[df3$band2 == "Red"] = c(640)
df3$band2[df3$band2 == "NIR"] = c(850)
df3$band2[df3$band2 == "SWIR1"] = c(1570)
df3$band2[df3$band2 == "SWIR2"] = c(2110)

df3$band2 = as.numeric(df3$band2)

df4 = df3 %>% 
  group_by(cultura, band, band2) %>% 
  summarise(mean = mean(value),
            low = quantile(value, 0.25),
            upper = quantile(value, 0.75))


curvl = ggplot(df4, aes(x=band2, y=mean, col = cultura))+
  geom_point(size = 3, alpha = 0.7)+
  geom_line(size = 1, alpha = 0.7)+
  geom_errorbar(aes(ymin = low, ymax = upper), width = 50)+
  geom_vline(xintercept=c(450, 850, 1570), linetype="dashed", color = "darkgray")+
  annotate("text", x = c(430, 850, 1570), y = 0.4, col = "#969696", hjust = -0.4,
           label= c("Visible", "Near InfraRed", "Mid InfraRed"))+
  scale_color_manual(values = c("#9932cc", "#9065d0", "#32a65e", "#edde8e", "#e6ccff"),
                     name = "Culture")+
  labs(x="Wavelenght (nm)", y = "Mean Reflectance (%)", title = "Spectral Reflectance Curve from Landsat-8")+
  theme_bw()


#ggsave(plot = curvl, filename = "Spect_Curve_Landsat8.png",
#       path = "C:/Users/Workshop/Documents/Research/Postdoc_UFRA/Papers/Culturas_permanentes/Figuras",
#       height = 7.5, width = 15, units = "cm", dpi = 300)



#Exploratory statistics --------------------------------------------------------
head(df2)

dfx = df2 %>% 
  na.omit() %>% 
  group_by(cultura, band) %>% 
  filter(band %in% c("EVI", "NDII", "NDVI")) %>% 
  summarise(mean = mean(value),
            min = min(value),
            max = max(value),
            low = quantile(value, 0.25),
            upper = quantile(value, 0.75))

view(dfx)

dfx2 = dfx[,c(1:3)]
dfx2 = dfx2 %>% spread(band, mean)
view(dfx2)
