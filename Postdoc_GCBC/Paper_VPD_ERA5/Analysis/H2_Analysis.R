#Paper VPD ERA5 Amazon - Analysis
# H2 - Along the successional gradient, young secondary forests
#remain exposed for longer periods to microclimatic conditions
#that favor fire spread throughout the dry season.

#Eduardo Q Marques 25-08-2026

library(tidyverse)

#Load data ---------------------------------------------------------------------
setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/H2")
dir()

df_full = read_csv("Hours_VPD75_Age_full.csv")
df = read_csv("Hours_VPD75_Age.csv")


#Graphics ----------------------------------------------------------------------
ggplot(df_full, aes(x=Age, y=Hours))+
  #geom_point()+
  geom_smooth(method = "lm")

ggplot(df_full, aes(x=Age, y=Hours, col=Month))+
  #geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month, scale = "free")


ggplot(df, aes(x=Age, y=Hours))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(df, aes(x=Age, y=Hours, col=Month))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month, scale = "free")


df2 = df |> 
  na.omit() |> 
  group_by(Age, Month) |> 
  summarise(Hours = median(Hours),
            n = n())

ggplot(df2, aes(x=Age, y=Hours))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(df2, aes(x=Age, y=Hours, col=Month))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Month, scale = "free")


#Logaritmar os dados para rodar o modelo...









