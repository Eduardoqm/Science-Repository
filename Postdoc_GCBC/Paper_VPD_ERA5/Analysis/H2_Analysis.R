#Paper VPD ERA5 Amazon - Analysis
# H2 - Along the successional gradient, young secondary forests
#remain exposed for longer periods to microclimatic conditions
#that favor fire spread throughout the dry season.

#Eduardo Q Marques 25-08-2026

library(tidyverse)

#Load data ---------------------------------------------------------------------
setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/H2")
dir()

df24 = read_csv("Hours_VPD75_Age_full.csv")


#Graphics ----------------------------------------------------------------------
ggplot(df24, aes(x=Age, y=Hours))+
  #geom_point()+
  geom_smooth(method = "lm")

ggplot(df24, aes(x=Age, y=Hours, col=Month))+
  #geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month, scale = "free")




df24$cond = df24$Month
df24$cond[df24$cond %in% c("Dec", "Jan", "Fev", "Mar", "April", "May")] <- "Rainy Season"
df24$cond[df24$cond != "Rainy Season"] <- "Dry Season"


df2 = df24 |> 
  na.omit() |> 
  group_by(Age, cond) |> 
  summarise(Hours = mean(Hours),
            n = n())

ggplot(df2, aes(x=Age, y=Hours))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~cond, scale = "free")


#Logaritmar os dados para rodar o modelo...









