#----------------------------------
#Wind Speed (Tower) X CHIRPS (2019)
#----------------------------------
#Eduardo Q Marques 17-03-2022
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

torre = torre %>% 
  filter(y == 2019)

torre$datetime = substr(torre$datetime, 1, 10)

torre = torre %>%
  group_by(datetime) %>% 
  summarise(max_speed = max(max_speed))

torre$datetime = as.Date(torre$datetime)
torre$datetime = yday(torre$datetime)
torre$max_speed = torre$max_speed*3.6 #Convert to km/h

colnames(torre) = c("date", "wind")

#CHIRPS ========================================================================
chp19 = read.csv("CHIRPS_2019.csv", sep = ",")

chp19max = chp19 %>% 
  na.omit() %>% 
  group_by(date) %>% 
  summarise(prec = max(prec))

chp19sum = chp19 %>% 
  na.omit() %>% 
  group_by(date) %>% 
  summarise(prec = sum(prec))

chp19mean = chp19 %>% 
  na.omit() %>% 
  group_by(date) %>% 
  summarise(prec = mean(prec))

chp19median = chp19 %>% 
  na.omit() %>% 
  group_by(date) %>% 
  summarise(prec = median(prec))

#Plot data =====================================================================
ggplot(torre, aes(x=date, y=wind))+
  geom_line(size = 1, col = "darkgreen")+
  theme_minimal()+
  xlab(NULL)+
  ylab("Wind Speed (km/h)")+
  theme(axis.text.x = element_text(angle = 90, hjust=1), legend.position=c(.20,.75))

ggplot(chp19max, aes(x=date, y=prec))+
  geom_line(size = 1, col = "darkblue")+
  theme_minimal()+
  xlab(NULL)+
  ylab("CHIRPS (mm)")+
  theme(axis.text.x = element_text(angle = 90, hjust=1), legend.position=c(.20,.75))

#Join data =====================================================================
dfmax = full_join(torre, chp19max, by = "date")
dfsum = full_join(torre, chp19sum, by = "date")
dfmean = full_join(torre, chp19mean, by = "date")
dfmedian = full_join(torre, chp19median, by = "date")


ggplot(dfmax, aes(x=prec, y=wind))+
  geom_point(aes(col = date), alpha = 0.7, size = 2)+
  geom_smooth(method = "lm", col = "royalblue")+
  stat_cor(show.legend = F)+
  labs( x = "Precipitation (mm)", y = "Wind Speed (km/h)",
       title = "CHIRPS (max in buffer) vs Wind Speed (Tower) - 2019")+
  scale_color_viridis()+
  theme_bw()


ggplot(dfsum, aes(x=wind, y=prec, col = date))+
  geom_point(alpha = 0.7)+
  geom_smooth(method = "lm")+
  stat_cor(show.legend = F)+
  labs(x = "Wind Speed (km/h)", y = "Precipitation (mm)",
       title = "CHIRPS (sum values in buffer) vs Wind Speed (Tower)")+
  theme_minimal()


ggplot(dfmean, aes(x=wind, y=prec, col = date))+
  geom_point(alpha = 0.7)+
  geom_smooth(method = "lm")+
  stat_cor(show.legend = F)+
  labs(x = "Wind Speed (km/h)", y = "Precipitation (mm)",
       title = "CHIRPS (mean value in buffer) vs Wind Speed (Tower)")+
  theme_minimal()


ggplot(dfmedian, aes(x=wind, y=prec, col = date))+
  geom_point(alpha = 0.7)+
  geom_smooth(method = "lm")+
  stat_cor(show.legend = F)+
  labs(x = "Wind Speed (km/h)", y = "Precipitation (mm)",
       title = "CHIRPS (median value in buffer) vs Wind Speed (Tower)")+
  theme_minimal()






