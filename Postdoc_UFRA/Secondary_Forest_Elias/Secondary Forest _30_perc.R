#Secondary Forest > 30% (Elias Paper)

#Eduardo Q Marques 12-03-2025

library(terra)
library(sf)
library(tidyverse)
library(lme4)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Eduardo/Documents/Analises_Elias/Rasters")
dir()

sec = rast("Perc_SecForest_1km.tif")
pri = rast("Perc_PriForest_1km.tif")
mage = rast("Mean_Forest_age_1km.tif")
agri = rast("Perc_Agriculture_1km.tif")
et = rast("ET_Amazonia_2023_1km.tif")
lst = rast("LST_Amazonia_2023_1km.tif")
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

df = getv(df, pri); colnames(df)[1] = c("Perc_priforest")
df = getv(df, agri); colnames(df)[1] = c("Perc_agriculture")
df = getv(df, mage); colnames(df)[1] = c("Age_secforest")
df = getv(df, et); colnames(df)[1] = c("ET")
df = getv(df, lst); colnames(df)[1] = c("LST")

sf_use_s2(FALSE)
df = st_intersection(amr[1], df); colnames(df)[1] = c("Regions")
df$Regions = substr(df$Regions, 1, 2)
df$Age_secforest = round(df$Age_secforest, 0)

plot(df)
names(df)

#Preparing data to analysis ----------------------------------------------------
df2=df%>%
  na.omit() %>% 
  dplyr::filter(LST>=24) %>% 
  st_drop_geometry()%>%
  group_by(Regions, Age_secforest) %>% 
  filter(Age_secforest >4) %>% 
  filter(Age_secforest <36) %>% 
  summarise(LST = mean(LST),
            ET = mean(ET),
            Perc_agriculture = mean(Perc_agriculture),
            Perc_priforest = mean(Perc_priforest),
            Perc_secforest = mean(Perc_priforest))

#Exploratory Graphics ----------------------------------------------------------
ggplot(df2, aes(x = Age_secforest, y = LST))+
  geom_point(aes(colour = Perc_agriculture))+
  geom_smooth(method = "lm")+
  facet_wrap(Regions~.,scales = 'free')+
  scale_color_gradient(low='blue',high = 'yellow')+
  theme_minimal()

ggplot(df2, aes(x = Age_secforest, y = ET))+
  geom_point(aes(colour = Perc_agriculture))+
  geom_smooth(method = "lm")+
  facet_wrap(Regions~.,scales = 'free')+
  scale_color_gradient(low='blue',high = 'yellow')+
  theme_minimal()

#Models ------------------------------------------------------------------------
#LST Model
m_lst=lmer(LST~Age_secforest*Regions+(1|Perc_agriculture)+(1|Perc_priforest),data=df2)
summary(m_lst)
MuMIn::r.squaredGLMM(m_lst)

df2$pred_lst=predict(m_lst)

ggplot(df2, aes(x = Age_secforest, y = LST))+
  geom_point(aes(colour = Perc_agriculture),alpha=0.1)+
  geom_smooth(aes(x=Age_secforest, y=pred_lst))+
  facet_wrap(Regions~.,scales = 'free')+
  scale_color_gradient(low='darkgreen',high = 'red')


#ET Model
m_et=lmer(ET~Age_secforest*Regions+(1|Perc_agriculture)+(1|Perc_priforest),data=df2)
summary(m_et)
MuMIn::r.squaredGLMM(m_et)

df2$pred_et=predict(m_et)

ggplot(df2, aes(x = Age_secforest, y = ET))+
  geom_point(aes(colour = Perc_agriculture),alpha=0.1)+
  geom_smooth(aes(x=Age_secforest, y=pred_et))+
  facet_wrap(Regions~.,scales = 'free')+
  scale_color_gradient(low='darkgreen',high = 'red')





