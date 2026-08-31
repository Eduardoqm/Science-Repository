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

gg = ggplot(df2, aes(x=Age, y=Hours, col = cond))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Age", y = "Hours per month (VPD ≥ 0.75 kPa)", col = NULL)+
  facet_wrap(~cond, scales = "free")+
  theme_bw()+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold")); gg

ggsave(gg, filename = "Hours_VPD75_Age.png",
       dpi = 600, units = "cm", height = 7, width = 14)


#Logaritmar os dados para rodar o modelo...
model1 <- lm(Hours ~ Age * cond, data = df2)
summary(model1)
