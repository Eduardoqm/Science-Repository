#----------------------------------
#Wind Speed (Tower) X CHIRPS
#----------------------------------
#Eduardo Q Marques 28-03-2022
#eduardobio2009@gmail.com
#----------------------------------

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(viridis)
library(fmsb)
library(lubridate)

#Tower data ====================================================================
setwd("~/Data/Tanguro Tower data")
torre = read.csv("Dados_Vento_Torre_Controle.csv", sep = ",")

#torre = torre %>% 
  #filter(y == 2019)

torre$datetime = substr(torre$datetime, 1, 10)

torre = torre %>%
  group_by(datetime, y) %>% 
  summarise(max_speed = max(max_speed))

torre$datetime = as.Date(torre$datetime)
torre$datetime = yday(torre$datetime)
torre$max_speed = torre$max_speed*3.6 #Convert to km/h

colnames(torre) = c("date", "year", "wind")

#CHIRPS ========================================================================
chp = read.csv("CHIRPS_2012-2020.csv", sep = ",")

chpmax = chp %>% 
  na.omit() %>% 
  group_by(date, year) %>% 
  summarise(prec = max(prec))

#Plot data =====================================================================
ggplot(torre, aes(x=date, y=wind))+
  geom_line(size = 1)+
  facet_wrap(~year)+
  xlab(NULL)+
  ylab("Wind Speed (km/h)")

ggplot(chpmax, aes(x=date, y=prec))+
  geom_line(size = 1)+
  facet_wrap(~year)+
  xlab(NULL)+
  ylab("CHIRPS (mm)")

#Join data =====================================================================
dfmax = full_join(torre, chpmax, by = c("date", "year"))

a = ggplot(dfmax, aes(x=prec, y=wind))+
  geom_point(aes(col = year), alpha = 0.7, size = 2)+
  geom_smooth(method = "lm", col = "royalblue")+
  stat_cor(show.legend = F)+
  labs(x = "Daily accumulated precipitation (mm)", y = "Maximum Wind Speed per day (km/h)",
        title = "CHIRPS (max in buffer) vs Wind Speed (Tower)")+
  scale_color_viridis()+
  theme_bw()

b = ggplot(dfmax, aes(x=prec, y=wind))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Daily accumulated precipitation (mm)", y = "Maximum Wind Speed per day (km/h)")+
  facet_wrap(~year)+
  scale_color_viridis()+
  theme_bw()

#ggsave(filename = "WD_tower-Prec_CHIRPS_allyears.png", plot = a,
 #     path = "~/Figures", width = 15, height = 12, units = "cm", dpi = 300)

#ggsave(filename = "WD_tower-Prec_CHIRPS_facet_allyears.png", plot = b,
 #     path = "~/Figures", width = 20, height = 12, units = "cm", dpi = 300)

dfmax = dfmax %>% filter(year %in% c(2014,2014,2016,2017,2018,2019,2020))

a = ggplot(dfmax, aes(x=prec, y=wind))+
  geom_point(aes(col = year), alpha = 0.7, size = 2)+
  geom_smooth(method = "lm", col = "royalblue")+
  stat_cor(show.legend = F)+
  labs(x = "Daily accumulated precipitation (mm)", y = "Maximum Wind Speed per day (km/h)",
       title = "CHIRPS (max in buffer) vs Wind Speed (Tower)")+
  scale_color_viridis()+
  theme_bw()

b = ggplot(dfmax, aes(x=prec, y=wind))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Daily accumulated precipitation (mm)", y = "Maximum Wind Speed per day (km/h)")+
  facet_wrap(~year)+
  scale_color_viridis()+
  theme_bw()

#ggsave(filename = "WD_tower-Prec_CHIRPS.png", plot = a,
 #      path = "~/Figures", width = 15, height = 12, units = "cm", dpi = 300)

#ggsave(filename = "WD_tower-Prec_CHIRPS_facet.png", plot = b,
 #      path = "~/Figures", width = 20, height = 12, units = "cm", dpi = 300)








