#Toy H3

#Eduardo Q Marques 10-03-2026

library(tidyverse)

setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/Toy_H3")
dir()

df = read.csv("ERA5_CP_VPD_since1950.csv"); df = df[,-4]
head(df)

df$year = substr(df$system.index, 1, 4)
df$month = substr(df$system.index, 5, 6)
df$day = substr(df$system.index, 7, 8)
df$hour = substr(df$system.index, 10, 11)


df$cond = "Dry Season"
rain_months = c("12", "01", "02", "03", "04", "05")

for (z in rain_months) {
  df$cond[df$month == z] = "Rainy Season"
}

df$year = as.numeric(df$year)
head(df)

#Itensity
#model1 <- lm(Intesidade ~ Ano * Season + Ano * Região)
df2 = df %>% 
  group_by(year, cond) %>% 
  summarise(VPD_int = mean(VPD))

model1 <- lm(VPD_int ~ year * cond, data = df2)
summary(model1)

ggplot(df2, aes(x=year, y=VPD_int, col = cond))+
  geom_point()+
  geom_smooth(method = "lm")

#Flamability duration
#model1 <- lm(Duração ~ Ano * Season + Ano * Região)
df$contagem = 1
df3 = df %>% 
  group_by(year, cond) %>% 
  filter(VPD >= 0.75) %>% 
  summarise(VPD_time = sum(contagem))


model2 <- lm(VPD_time ~ year * cond, data = df3)
summary(model2)

ggplot(df3, aes(x=year, y=VPD_time, col = cond))+
  geom_point()+
  geom_smooth(method = "lm")








