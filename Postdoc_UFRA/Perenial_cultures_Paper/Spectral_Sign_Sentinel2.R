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

#Sentinel-2 reflectance
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


df2 = df %>% 
  select(cultura, Blue, Green, Red,
         RedEdge1, RedEdge2, RedEdge3,
         NIR, NIRn, SWIR1, SWIR2) %>% 
  mutate(NDVI = ((NIR-Red)/(NIR+Red)),
         NDII = ((NIR-SWIR1)/(NIR+SWIR1)),
         EVI = 2.5*((NIR-Red)/(NIR+6*Red-7.5*Blue+1)),
         RENDVI = ((NIR-RedEdge1)/(NIR+RedEdge1)),
         Chl_RedEdge = (NIR/RedEdge1)-1) %>% 
  gather(band, value, -cultura)


#Plotting Results --------------------------------------------------------------
splot = ggplot(df2, aes(x=cultura, y=value, fill=cultura))+
  geom_boxplot(alpha = 0.9)+
  #geom_violin()+
  facet_wrap(~factor(band,
             levels = c("Blue", "Green", "Red",
                        "RedEdge1", "RedEdge2", "RedEdge3",
                        "NIR", "NIRn", "SWIR1", "SWIR2",
                        "EVI", "NDVI", "NDII",
                        "RENDVI", "Chl_RedEdge")),
             scale = "free", ncol = 5)+
  scale_fill_manual(values = c("#9932cc", "#9065d0", "#32a65e", "#edde8e", "#e6ccff"))+
  labs(x=NULL, y = NULL, title = "Spectral Signature from Sentinel-2")+
  theme_bw()+
  theme(legend.position = c(100, 100),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))


#ggsave(plot = splot, filename = "Spect_Sign_Sentinel2.png",
#       path = "C:/Users/Workshop/Documents/Research/Postdoc_UFRA/Papers/Culturas_permanentes/Figuras",
#       height = 17, width = 25, units = "cm", dpi = 300)


#Spectral Reflectance curve ----------------------------------------------------
df3 = df %>% 
  select(cultura, Blue, Green, Red,
         RedEdge1, RedEdge2, RedEdge3,
         NIR, NIRn, SWIR1, SWIR2) %>% 
  gather(band, value, -cultura)

df3$band2 = df3$band

#df3$band2[df3$band2 == "Ultra_Blue"] = c(433)
df3$band2[df3$band2 == "Blue"] = c(458)
df3$band2[df3$band2 == "Green"] = c(543)
df3$band2[df3$band2 == "Red"] = c(650)
df3$band2[df3$band2 == "RedEdge1"] = c(698)
df3$band2[df3$band2 == "RedEdge2"] = c(733)
df3$band2[df3$band2 == "RedEdge3"] = c(773)
df3$band2[df3$band2 == "NIR"] = c(785)
df3$band2[df3$band2 == "NIRn"] = c(855)
df3$band2[df3$band2 == "SWIR1"] = c(1565)
df3$band2[df3$band2 == "SWIR2"] = c(2100)

df3$band2 = as.numeric(df3$band2)

df4 = df3 %>% 
  na.omit() %>% 
  group_by(cultura, band, band2) %>% 
  summarise(mean = mean(value),
            low = quantile(value, 0.25),
            upper = quantile(value, 0.75))


curvs = ggplot(df4, aes(x=band2, y=mean, col = cultura))+
  geom_point(size = 3, alpha = 0.7)+
  geom_line(size = 1, alpha = 0.7)+
  geom_errorbar(aes(ymin = low, ymax = upper), width = 50)+
  geom_vline(xintercept=c(458, 698, 785, 1565), linetype="dashed", color = "darkgray")+
  annotate("text", x = c(433, 615, 785, 1565), y = c(0.4, 0.05, 0.4, 0.4),
           col = "#969696", hjust = -0.3,
           label= c("Visible", "RedEdge", "Near InfraRed", "Mid InfraRed"))+
  scale_color_manual(values = c("#9932cc", "#9065d0", "#32a65e", "#edde8e", "#e6ccff"),
                     name = "Culture")+
  labs(x="Wavelenght (nm)", y = "Mean Reflectance (%)",
       title = "Spectral Reflectance Curve from Sentinel-2")+
  theme_bw()


#ggsave(plot = curvs, filename = "Spect_Curve_Sentinel2.png",
#       path = "C:/Users/Workshop/Documents/Research/Postdoc_UFRA/Papers/Culturas_permanentes/Figuras",
#       height = 7.5, width = 15, units = "cm", dpi = 300)

#Exploratory statistics --------------------------------------------------------
head(df2)

dfx = df2 %>% 
  na.omit() %>% 
  group_by(cultura, band) %>% 
  filter(band %in% c("Chl_RedEdge", "EVI", "NDII",
                     "NDVI", "RENDVI")) %>% 
  summarise(mean = mean(value),
            min = min(value),
            max = max(value),
            low = quantile(value, 0.25),
            upper = quantile(value, 0.75))

view(dfx)

dfx2 = dfx[,c(1:3)]
dfx2 = dfx2 %>% spread(band, mean)
view(dfx2)













