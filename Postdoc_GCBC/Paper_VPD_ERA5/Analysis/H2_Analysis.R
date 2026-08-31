#Paper VPD ERA5 Amazon - Analysis
# H2 - Along the successional gradient, young secondary forests
#remain exposed for longer periods to microclimatic conditions
#that favor fire spread throughout the dry season.

#Eduardo Q Marques 25-08-2026

library(tidyverse)

#Load data ---------------------------------------------------------------------
setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/H2")
dir()

df23 = read_csv("Hours_VPD75_Age_full_2023.csv")
df24 = read_csv("Hours_VPD75_Age_full_2024.csv")
df25 = read_csv("Hours_VPD75_Age_full_2025.csv")

df = rbind(df23, df24, df25)

#Graphics ----------------------------------------------------------------------
ggplot(df, aes(x=Age, y=Hours))+
  geom_smooth(method = "lm")

ggplot(df, aes(x=Age, y=Hours, col=Month))+
  geom_smooth(method = "lm")+
  facet_wrap(~Month, scale = "free")

df$cond = df$Month
df$cond[df$cond %in% c("Dec", "Jan", "Fev", "Mar", "April", "May")] <- "Rainy Season"
df$cond[df$cond != "Rainy Season"] <- "Dry Season"

ggplot(df, aes(x=Age, y=Hours))+
  geom_smooth(method = "lm")+
  facet_wrap(~cond, scale = "free")


df2 = df |> 
  na.omit() |> 
  group_by(Age, cond) |> 
  summarise(Hours = mean(Hours),
            n = n())

ggplot(df2, aes(x=Age, y=Hours))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~cond, scale = "free")


#Logaritmar os dados para rodar o modelo...
model1 <- lm(Hours ~ Age * cond, data = df2)
summary(model1)
