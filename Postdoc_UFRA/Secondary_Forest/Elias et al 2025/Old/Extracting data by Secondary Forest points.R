#Secondary Forest > 30% (Elias Paper)

#Eduardo Q Marques 12-03-2025

library(terra)
library(sf)
library(tidyverse)
library(lme4)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serra (Elias et al)/Analises_Elias/Rasters")
dir()

sec = rast("Perc_SecForest_1km.tif")
pri = rast("Perc_PriForest_1km.tif")
mage = rast("Mean_Forest_age_1km.tif")
agri = rast("Perc_Agriculture_1km.tif")
water = rast("Perc_Water_1km.tif")
nforest = rast("Perc_Natural_No_Forest_1km.tif")
nveg = rast("Perc_Non_Vegetated_1km.tif")

et = rast("ET_Amazonia_2023_1km.tif")
lst = rast("LST_Amazonia_2023_1km.tif")

amr = read_sf("G:/Meu Drive/Postdoc_UFRA/Papers/Serra (Elias et al)/Analises_Elias/Shapes")

#Binary filtering > 30% of secondary forest ------------------------------------
sec2 = sec
sec2[sec2 < 1] = NA

df = st_as_sf(as.points(sec2, na.rm = T))
colnames(df)[1] = c("Perc_secforest")

#Extracting others information -------------------------------------------------
getv = function(z, x){
  dfx = terra::extract(x, z)
  z = cbind(dfx[,2], z)
}

df = getv(df, pri); colnames(df)[1] = c("Perc_allforest")
df = getv(df, agri); colnames(df)[1] = c("Perc_agriculture")
df = getv(df, mage); colnames(df)[1] = c("Age_secforest")
df = getv(df, water); colnames(df)[1] = c("Water")
df = getv(df, nforest); colnames(df)[1] = c("Nat_non_forest")
df = getv(df, nveg); colnames(df)[1] = c("Non_Vegetated")
df = getv(df, et); colnames(df)[1] = c("ET")
df = getv(df, lst); colnames(df)[1] = c("LST")

sf_use_s2(FALSE)
df = st_intersection(amr[1], df); colnames(df)[1] = c("Regions")
df$Regions = substr(df$Regions, 1, 2)
df$Age_secforest = round(df$Age_secforest, 0)

plot(df)
names(df)

#Save --------------------------------------------------------------------------
dfz = df %>% 
  st_drop_geometry()

setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serra (Elias et al)/Analises_Elias/Dados")
write.csv(dfz, "Paper_Elias_Data.csv", row.names = F)

#Preparing data to analysis ----------------------------------------------------
df2=df%>%
  na.omit() %>% 
  dplyr::filter(LST>=24) %>% 
  filter(Water < 5) %>% 
  filter(Non_Vegetated < 5) %>%
  filter(Nat_non_forest < 5) %>% 
  st_drop_geometry()%>%
  group_by(Regions, Perc_secforest) %>% 
#  filter(Age_secforest >4) %>% 
#  filter(Age_secforest <36) %>% 
  summarise(LST = mean(LST),
            ET = mean(ET),
            Perc_agriculture = mean(Perc_agriculture),
            Perc_allforest = mean(Perc_allforest),
            Age_secforest = mean(Age_secforest))

#Exploratory Graphics ----------------------------------------------------------
ggplot(df2, aes(x = Perc_secforest, y = LST))+
  geom_point(aes(colour = Perc_priforest))+
  geom_smooth(method = "lm")+
  facet_wrap(Regions~.,scales = 'free')+
  scale_color_gradient(low='blue',high = 'yellow')+
  theme_minimal()

ggplot(df2, aes(x = Perc_secforest, y = ET))+
  geom_point(aes(colour = Perc_priforest))+
  geom_smooth(method = "lm")+
  facet_wrap(Regions~.,scales = 'free')+
  scale_color_gradient(low='blue',high = 'yellow')+
  theme_minimal()





