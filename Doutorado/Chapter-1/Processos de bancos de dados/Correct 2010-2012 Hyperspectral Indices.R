#=========================================#
# Correct 2010-2012 Hyperspectral Indices #
#                                         #
# Eduardo Q Marques 05-10-2020            #
# Update: 15-04-2021                      #
#=========================================#


library(stats)
library(tidyverse)
library(reshape2)

#Data =============================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')
df$index = as.character(df$index)

#Explore data from water indices ------------------------------------------------------------------
df %>% 
  filter(index == "wbi") %>% 
  filter(year == 2012) %>% 
  ggplot(aes(x=x, y=y, fill = value))+
  geom_raster()

df %>% 
  filter(index == "ndwi") %>%
  filter(year == 2012) %>%
  ggplot(aes(x=x, y=y, fill = value))+
  geom_raster()

df %>% 
  filter(index == "ndwi") %>%
  filter(value <= 0.1) %>%
  filter(year == 2010) %>%
  ggplot(aes(x=x, y=y, fill = value))+
  geom_raster()

df %>% 
  filter(index == "ndwi") %>%
  filter(value > 0.1) %>%
  filter(year == 2010) %>%
  ggplot(aes(x=x, y=y, fill = value))+
  geom_raster()

#Getting incorrect data info ----------------------------------------------------------------------
incor = df %>% 
  filter(index == "ndwi") %>%
  filter(value > 0.1) %>%
  filter(year %in% c(2010, 2012)) %>% 
  unite("id", c("x", "y","year"), sep = "_")

#Change incorrect data to NA ----------------------------------------------------------------------
df2 = df %>% 
  unite("id", c("x", "y","year"), sep = "_")

for (a in incor$id) {
  df2$value[df2$id == a] <- NA
}

df2 = df2 %>% 
  separate(id, c("x", "y","year"), sep = "_")

df2$x = as.numeric(df2$x)
df2$y = as.numeric(df2$y)
df2$year = as.numeric(df2$year)

#Plot new data to verify --------------------------------------------------------------------------
df2 %>% 
  filter(index == "wbi") %>%
  filter(year == 2012) %>%
  ggplot(aes(x=x, y=y, fill = value))+
  geom_raster()

df2 %>% 
  filter(index == "ndwi") %>%
  filter(value <= 0.1) %>%
  filter(year == 2010) %>%
  ggplot(aes(x=x, y=y, fill = value))+
  geom_raster()

df2 %>% 
  filter(index == "ndwi") %>%
  filter(value > 0.1) %>%
  filter(year == 2010) %>%
  ggplot(aes(x=x, y=y, fill = value))+
  geom_raster()

#Export new data ----------------------------------------------------------------------------------

#AFTER EXPORT NEED TO REMOVE WRONG VALUES ON THE TABLE!

#setwd("~/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
#write.csv(df2, "Hyperion_indexs_all_xy-B.csv", row.names = F)


















