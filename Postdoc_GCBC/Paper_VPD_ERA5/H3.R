# H3 - The increase in VPD has been more pronounced
# in southern Amazon than in central Amazon over
# the past five decades.

#Eduardo Q Marques 10-03-2026

library(tidyverse)

setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/H3")
dir()

df = read_csv("ERA5_CP_VPD_since1975.csv")
head(df)

df = df[,c(4,2,3)]; head(df)

df$year = substr(df$datetime, 1, 4)
df$month = substr(df$datetime, 6, 7)
df$day = substr(df$datetime, 9, 10)
df$hour = substr(df$datetime, 12, 13)
df$Region = substr(df$Source, 1, 2)
df = df[,-2]; head(df)

df$cond = "Dry Season"
rain_months = c("12", "01", "02", "03", "04", "05")

for (z in rain_months) {
  df$cond[df$month == z] = "Rainy Season"
}

df$year = as.numeric(df$year); head(df)

#Itensity ----------------------------------------------------------------------
#model1 <- lm(Intesidade ~ Ano * Season + Ano * Região)
df2 = df %>% 
  group_by(Region, year, cond) %>% 
  summarise(VPD_int = mean(VPD))
head(df2)

model1 <- lm(VPD_int ~ year * cond + year * Region, data = df2)
summary(model1)

ggplot(df2, aes(x=year, y=VPD_int, col = cond))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = NULL, y = "Mean VPD (kPa)", col = NULL)+
  facet_wrap(~factor(Region, c("NW","NE","SW","SE")), scales = "free")+
  theme_bw()

#Flamability duration ----------------------------------------------------------
#model1 <- lm(Duração ~ Ano * Season + Ano * Região)
df$contagem = 1
df3 = df %>% 
  group_by(Region, year, cond) %>% 
  filter(VPD >= 0.75) %>% 
  summarise(VPD_time = (sum(contagem)/30)/24)
head(df3)

model2 <- lm(VPD_time ~ year * cond + year * Region, data = df3)
summary(model2)

#Residual standard error: 261.3 on 398 degrees of freedom
#Multiple R-squared:  0.8401,	Adjusted R-squared:  0.8365 
#F-statistic: 232.4 on 9 and 398 DF,  p-value: < 2.2e-16



ggplot(df3, aes(x=year, y=VPD_time, col = cond))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = NULL, y = "Number of hours (VPD ≥ 0.75 kPa)", col = NULL)+
  facet_wrap(~factor(Region, c("NW","NE","SW","SE")), scales = "free")+
  theme_bw()




