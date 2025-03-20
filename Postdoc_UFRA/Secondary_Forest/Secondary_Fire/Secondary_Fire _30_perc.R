#Secondary Fire > SF30% (Congress of Botany)

#Eduardo Q Marques 19-03-2025

library(terra)
library(sf)
library(tidyverse)
library(lme4)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Geodata/Rasters"); dir()

sec = rast("C:/Users/Eduardo/Documents/Analises_Elias/Rasters/Perc_SecForest_1km.tif")
mage = rast("C:/Users/Eduardo/Documents/Analises_Elias/Rasters/Mean_Forest_age_1km.tif")
freqf = rast("G:/Meu Drive/Postdoc_UFRA/Geodata/Rasters/MapBiomes_Brazil/Fire/Amazonia_frequency_burned-2018_2023.tif")
ndvi = rast("G:/Meu Drive/Postdoc_UFRA/Geodata/Rasters/Amazonia_NDVI_2023B.tif")
grnd = rast("G:/Meu Drive/Postdoc_UFRA/Geodata/Rasters/Amazonia_GRND_2023.tif")
ndwi = rast("G:/Meu Drive/Postdoc_UFRA/Geodata/Rasters/Amazonia_NDWI_2023.tif")
rendvi = rast("G:/Meu Drive/Postdoc_UFRA/Geodata/Rasters/Amazonia_RENDVI_2023.tif")

amr = read_sf("C:/Users/Eduardo/Documents/Analises_Elias/Shapes")

#Binary filtering > 30% of secondary forest ------------------------------------
sec2 = sec
sec2[sec2 < 30] = NA

df = st_as_sf(as.points(sec2, na.rm = T))
colnames(df)[1] = c("Perc_secforest")

#Extracting others information -------------------------------------------------
getv = function(z, x){
  dfx = terra::extract(x, z)
  z = cbind(dfx[,2], z)
}

df = getv(df, mage); colnames(df)[1] = c("Age_secforest")
df = getv(df, freqf); colnames(df)[1] = c("Fire_freq")
df = getv(df, ndvi); colnames(df)[1] = c("NDVI")
df = getv(df, grnd); colnames(df)[1] = c("GRND")
df = getv(df, ndwi); colnames(df)[1] = c("NDWI")
df = getv(df, rendvi); colnames(df)[1] = c("RENDVI")

sf_use_s2(FALSE)
df = st_intersection(amr[1], df); colnames(df)[1] = c("Regions")
df$Regions = substr(df$Regions, 1, 2)
df$Age_secforest = round(df$Age_secforest, 0)

plot(df)

#Preparing data to analysis ----------------------------------------------------
df$Age_secforest2 = "a"
df$Age_secforest2[df$Age_secforest < 11] = c("1-10y")
df$Age_secforest2[df$Age_secforest > 10] = c("+11y")

df2=df%>%
  st_drop_geometry()%>%
  filter(Age_secforest >2 & Age_secforest <36)%>%
  group_by(Regions,Age_secforest2,Fire_freq)%>%
  summarise(Perc_secforest = mean(Perc_secforest),
            NDVI = mean(NDVI),
            GRND = mean(GRND),
            NDWI = mean(NDWI),
            RENDVI = mean(RENDVI))%>% 
  ungroup()

#Models ------------------------------------------------------------------------
#NDVI
m_ndvi=lm(NDVI~Fire_freq+Regions, data=df3)
summary(m_ndvi)

#GRND
m_grnd=lm(GRND~Fire_freq+Regions, data=df3)
summary(m_grnd)

#NDWI
m_ndwi=lm(NDWI~Fire_freq+Regions, data=df3)
summary(m_ndwi)

#RENDVI
m_rendvi=lm(RENDVI~Fire_freq+Regions, data=df3)
summary(m_rendvi)

#Plotting results --------------------------------------------------------------
df3 = df2 %>% 
  pivot_longer(cols = c(NDVI, GRND, NDWI, RENDVI),
               names_to = "Indexs",
               values_to = "value")


ggplot(df3, aes(x = Fire_freq, y = value, group =Age_secforest2,
                color = Age_secforest2))+
  geom_point(size = 4)+
  geom_smooth(method = "lm")+
  labs(x = "Frequency of Fire", color = "Age of Second Forest")+
  #scale_color_manual(values = c('blue','yellow'))+
  #facet_wrap(Regions~.,scales = 'free')+
  facet_grid(Indexs~Regions, scales = "free")+
  theme_bw()
