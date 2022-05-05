#-------------------------------------------------------------------------
#Wind Speed (Tower) X Precipitation (Darro) - Quantile Regression
#-------------------------------------------------------------------------
#Eduardo Q Marques 12-04-2022
#eduardobio2009@gmail.com
#-------------------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(viridis)
library(fmsb)
library(lubridate)

#Tower data ====================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Dados das torres")

torre = read.csv("Dados_Vento_Torre_Controle.csv", sep = ",")

torre$datetime = substr(torre$datetime, 1, 10)

torre = torre %>%
  group_by(datetime) %>% 
  summarise(max_speed = max(max_speed))

torre$datetime = as.Date(torre$datetime)
#torre$max_speed = torre$max_speed*3.6 #Convert to km/h

colnames(torre) = c("date", "wind")

#Darro ========================================================================
darro = read.csv("Master_Estacao_Darro_2020.csv", sep = ",")

darro = darro %>% 
  select(Date, ppt)

darroppt = darro %>% 
  na.omit() %>% 
  group_by(Date) %>% 
  summarise(ppt = sum(ppt))

colnames(darroppt) = c("date", "ppt")
darroppt$date = as.Date(darroppt$date)

#Testing Normalize and transformation of data =====================================================================
df = full_join(torre, darroppt, by = "date")

df$id = c(1:length(df$date))
df$date = as.numeric(substr(df$date, 1, 4))
df = df %>% filter(date %in% c(2014,2014,2016,2017,2018,2019,2020))

a1 = ggplot(df)+geom_density(aes(x=wind), fill = "blue", alpha = 0.35)
b1 = ggplot(df)+geom_density(aes(x=ppt), fill = "red", alpha = 0.35)

#Using Scale---------------------------------------
#df$wind = scale(df$wind)
#df$ppt = scale(df$ppt)

#Min-Max normalization function--------------------
#min_max_norm <- function(x) {
# (x - min(x)) / (max(x) - min(x))
#}

#df <- as.data.frame(lapply(na.omit(df[1:3]), min_max_norm))

#Using log ----------------------------------------
df$wind = log(df$wind)
df$ppt = log(df$ppt)

#Using log10 ----------------------------------------
#df$wind = log10(df$wind)
#df$ppt = log10(df$ppt)

#Using Square root ----------------------------------
#df$wind = sqrt(df$wind)
#df$ppt = sqrt(df$ppt)

a2 = ggplot(df)+geom_density(aes(x=wind), fill = "blue", alpha = 0.35)
b2 = ggplot(df)+geom_density(aes(x=ppt), fill = "red", alpha = 0.35)

ggarrange(a1, a2, b1, b2, ncol = 2, nrow = 2)


#Quantile values ================================================================
library(quantreg)
fit = rq(wind ~ ppt, tau = c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9,.95), data = df)
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

#Plot Regeressions ===========================================================
wd1420 = ggplot(df, aes(x=ppt, y=wind))+
  geom_point(aes(col = date), alpha = 0.7, size = 2)+
  geom_smooth(method = "lm", col = "royalblue")+
  stat_cor(show.legend = F)+
  stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), col = "red", aplha = 0.3)+
  #geom_abline(data = df3, aes(intercept = Intercept, slope = Precipitation), col = "red", aplha = 0.3)+
  labs( x = "Daily accumulated precipitation", y = "Wind Speed per day",
        title = "Precipitation (Darro Station) vs Wind Speed (Tower)")+
  scale_color_viridis()+
  theme_bw(); wd1420

wd1420f = ggplot(df, aes(x=ppt, y=wind))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  #stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), show.legend = TRUE, col = "red", aplha = 0.3)+
  labs( x = "Daily accumulated precipitation", y = "Wind Speed per day")+
  facet_wrap(~date)+
  scale_color_viridis()+
  theme_bw(); wd1420f

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














