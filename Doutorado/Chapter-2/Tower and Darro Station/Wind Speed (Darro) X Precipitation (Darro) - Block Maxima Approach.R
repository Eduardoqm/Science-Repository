#-------------------------------------------------------------------------
#Wind Speed (Darro) X Precipitation (Darro) - Tail Dependence and block maxima approach
#-------------------------------------------------------------------------
#Eduardo Q Marques 06-05-2022
#eduardobio2009@gmail.com
#-------------------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(viridis)
library(fmsb)
library(lubridate)

#Darro data ====================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Dados das torres")

darro = read.csv("Master_Estacao_Darro_2020.csv", sep = ",")

darro = darro %>% 
  select(Date, windSpd, ppt)
colnames(darro) = c("date", "ws", "ppt")


#df = darro %>% 
#  na.omit() %>% 
#  group_by(date) %>% 
#  summarise(ws = max(ws), ppt = sum(ppt))

df = darro
df$date = as.Date(df$date)

df$date2 = as.numeric(substr(df$date, 1, 4))
df$month = as.character(substr(df$date, 6, 7))

df = df %>% filter(date2 %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020))
df = df %>% filter(month %in% c("10","11","12","01","02","03","04"))

df = df %>% filter(ppt <100) #Outlier maybe a error in registration
quantile(df$ppt, 0.95)
quantile(df$ws, 0.95)
#df = df %>% filter(ppt >= quantile(ppt, 0.95))



library(extRemes)

df <- blockmaxxer(df, blocks = df$date, which="ppt")


cor(df$ppt, df$ws)

plot(df$ppt, df$ws)

taildep(df$ppt, df$ws, 0.95)

taildep.test(df$ppt, df$ws) # Recall that the null hypothesis is tail dependence!



df$ws2 = log(df$ws)
df$ppt2 = log(df$ppt)



ggplot(df, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.5, size = 3, col = "royalblue")+
  geom_smooth(method = "gam", col = "black")+
  stat_cor(show.legend = F)+
  #stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), col = "red", aplha = 0.3)+
  #geom_abline(data = df3, aes(intercept = Intercept, slope = Precipitation), col = "red", aplha = 0.3)+
  labs( x = "Maximum Precipitation (mm/day)", y = "Maximun Wind Speed (m/s) - day",
        title = "Precipitation vs Wind Speed (Darro Station)")+
  scale_color_viridis()+
  theme_bw()

ggplot(df, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "gam", col = "black")+
  stat_cor(show.legend = F)+
  #stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), show.legend = TRUE, col = "red", aplha = 0.3)+
  labs( x = "Daily accumulated precipitation", y = "Wind Speed per day")+
  facet_wrap(~date2)+
  scale_color_viridis()+
  theme_bw()

ggplot(df, aes(x=ppt2, y=ws2, col = date2))+
  geom_point(alpha = 0.7, size = 2)+
  geom_smooth(method = "gam", col = "royalblue")+
  stat_cor(show.legend = F)+
  #stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), col = "red", aplha = 0.3)+
  #geom_abline(data = df3, aes(intercept = Intercept, slope = Precipitation), col = "red", aplha = 0.3)+
  labs( x = "Daily accumulated precipitation", y = "Wind Speed per day",
        title = "Precipitation (Darro Station) vs Wind Speed (Tower)")+
  scale_color_viridis()+
  theme_bw()

ggplot(df, aes(x=ppt2, y=ws2))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "gam", col = "black")+
  stat_cor(show.legend = F)+
  #stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), show.legend = TRUE, col = "red", aplha = 0.3)+
  labs( x = "Daily accumulated precipitation", y = "Wind Speed per day")+
  facet_wrap(~date2)+
  scale_color_viridis()+
  theme_bw()








