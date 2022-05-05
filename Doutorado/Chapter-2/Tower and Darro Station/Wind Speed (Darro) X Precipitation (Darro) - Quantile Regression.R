#-------------------------------------------------------------------------
#Wind Speed (Darro) X Precipitation (Darro) - Quantile Regression
#-------------------------------------------------------------------------
#Eduardo Q Marques 05-05-2022
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

df = darro %>% 
  na.omit() %>% 
  group_by(date) %>% 
  summarise(ws = max(ws), ppt = sum(ppt))

#df = darro
df$date = as.Date(df$date)

#Testing Normalize and transformation of data =====================================================================
#df$id = c(1:length(df$date))
df$date2 = as.numeric(substr(df$date, 1, 4))
#df = df %>% filter(date %in% c(2014,2014,2016,2017,2018,2019,2020))

#a1 = ggplot(df)+geom_density(aes(x=ws), fill = "blue", alpha = 0.35)
#b1 = ggplot(df)+geom_density(aes(x=ppt), fill = "red", alpha = 0.35)

#Using log ----------------------------------------
#df$wind = log(df$ws)
#df$ppt = log(df$ppt)

#Using Square root ----------------------------------
#df$wind = sqrt(df$wind)
#df$ppt = sqrt(df$ppt)

#a2 = ggplot(df)+geom_density(aes(x=ws), fill = "blue", alpha = 0.35)
#b2 = ggplot(df)+geom_density(aes(x=ppt), fill = "red", alpha = 0.35)

#ggarrange(a1, a2, b1, b2, ncol = 2, nrow = 2)

#Plot Regeressions ===========================================================
wd0420 = ggplot(df, aes(x=ppt, y=ws, col = date2))+
  geom_point(alpha = 0.7, size = 2)+
  geom_smooth(method = "lm", col = "royalblue")+
  stat_cor(show.legend = F)+
  stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), col = "red", aplha = 0.3)+
  #geom_abline(data = df3, aes(intercept = Intercept, slope = Precipitation), col = "red", aplha = 0.3)+
  labs( x = "Daily accumulated precipitation", y = "Wind Speed per day",
        title = "Precipitation (Darro Station) vs Wind Speed (Tower)")+
  scale_color_viridis()+
  theme_bw(); wd0420

wd0420f = ggplot(df, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  #stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), show.legend = TRUE, col = "red", aplha = 0.3)+
  labs( x = "Daily accumulated precipitation", y = "Wind Speed per day")+
  facet_wrap(~date2)+
  scale_color_viridis()+
  theme_bw(); wd0420f


#Removing 2004 to 2009 ========================================================
df = df %>% filter(date2 %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020))

wd1020 = ggplot(df, aes(x=ppt, y=ws, col = date2))+
  geom_point(alpha = 0.7, size = 2)+
  geom_smooth(method = "lm", col = "royalblue")+
  stat_cor(show.legend = F)+
  stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), col = "red", aplha = 0.3)+
  #geom_abline(data = df3, aes(intercept = Intercept, slope = Precipitation), col = "red", aplha = 0.3)+
  labs( x = "Daily accumulated precipitation", y = "Wind Speed per day",
        title = "Precipitation (Darro Station) vs Wind Speed (Tower)")+
  scale_color_viridis()+
  theme_bw(); wd1020

wd1020f = ggplot(df, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  #stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), show.legend = TRUE, col = "red", aplha = 0.3)+
  labs( x = "Daily accumulated precipitation", y = "Wind Speed per day")+
  facet_wrap(~date2)+
  scale_color_viridis()+
  theme_bw(); wd1020f
#Quantile values ================================================================
library(quantreg)
fit = rq(ws ~ ppt, tau = c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9,.95), data = df)
fit

df2 = fit$coefficients

df2 = melt(df2)
ppt = df2 %>% 
  filter(Var1 == "ppt")
inter = df2 %>% 
  filter(Var1 == "(Intercept)")

df3 = cbind(ppt, inter)
df3 = df3[,c(2,3,6)]
colnames(df3) = c("Quantile", "Precipitation", "Intercept")
df3$Quantile = as.numeric(substr(df3$Quantile, 6, 9))

quant = ggplot(df3)+
  geom_line(aes(x = Quantile, y = Precipitation), col = "blue", size = 1.5, alpha = 0.5)+
  geom_point(aes(x = Quantile, y = Precipitation), col = "blue", size = 2, alpha = 0.5)+
  #geom_line(aes(x = Quantile, y = Intercept), col = "red", size = 1.5, alpha = 0.5)+
  labs( x = "Wind Speed Quantiles (Max/day)", y = "Precipitation Slope (mm/day)")+
  theme_bw(); quant



#ggsave(filename = "WD_tower-Prec_darro_log.png", plot = wd1420,
#     path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 15, height = 12, units = "cm", dpi = 300)

#ggsave(filename = "WD_tower-Prec_darro_log_facet.png", plot = wd1420f,
#     path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 20, height = 12, units = "cm", dpi = 300)

#ggsave(filename = "WD_tower-Prec_darro_facet.png", plot = wd1420f,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 20, height = 12, units = "cm", dpi = 300)















