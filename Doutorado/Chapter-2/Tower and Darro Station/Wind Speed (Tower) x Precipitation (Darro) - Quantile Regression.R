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

#Join data =====================================================================
df = full_join(torre, darroppt, by = "date")

df$id = c(1:length(df$date))
df$date = as.numeric(substr(df$date, 1, 4))
df = df %>% filter(date %in% c(2014,2014,2016,2017,2018,2019,2020))

#Linear Regeressions ===========================================================
wd1420 = ggplot(df, aes(x=ppt, y=wind))+
  geom_point(aes(col = date), alpha = 0.7, size = 2)+
  geom_smooth(method = "lm", col = "royalblue")+
  stat_cor(show.legend = F)+
  labs( x = "Daily accumulated precipitation (mm)", y = "Maximum Wind Speed per day (m/s)",
        title = "Precipitation (Darro Station) vs Wind Speed (Tower)")+
  scale_color_viridis()+
  theme_bw(); wd1420

wd1420f = ggplot(df, aes(x=ppt, y=wind))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Daily accumulated precipitation (mm)", y = "Maximum Wind Speed per day (m/s)")+
  facet_wrap(~date)+
  scale_color_viridis()+
  theme_bw(); wd1420f


#ggsave(filename = "WD_tower-Prec_darro.png", plot = wd1420,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 15, height = 12, units = "cm", dpi = 300)

#ggsave(filename = "WD_tower-Prec_darro_facet.png", plot = wd1420f,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 20, height = 12, units = "cm", dpi = 300)

#Quantile Regression ============================================================
library(quantreg)

ggplot(df, aes(x=ppt, y=wind))+
  geom_point(aes(col = date), alpha = 0.7, size = 2)+
  geom_smooth(method = "lm", col = "royalblue")+
  #geom_quantile()+
  stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), show.legend = TRUE, col = "red", size = 1, aplha = 0.3)+
  #stat_cor(show.legend = F)+
  labs( x = "Daily accumulated precipitation (mm)", y = "Maximum Wind Speed per day (m/s)",
        title = "Precipitation (Darro Station) vs Wind Speed (Tower)")+
  scale_color_viridis()+
  theme_bw()
































































