#----------------------------------
#Wind Speed (Tower) X Precipitation (Darro)
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
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Dados das torres")

torre = read.csv("Dados_Vento_Torre_Controle.csv", sep = ",")

#torre = torre %>% 
 # filter(y == 2019)

torre$datetime = substr(torre$datetime, 1, 10)

torre = torre %>%
  group_by(datetime) %>% 
  summarise(max_speed = max(max_speed))

torre$datetime = as.Date(torre$datetime)
#torre$datetime = yday(torre$datetime)
torre$max_speed = torre$max_speed*3.6 #Convert to km/h

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

#Plot data =====================================================================
ggplot(torre, aes(x=date, y=wind))+
  geom_line(size = 1, col = "darkgreen")+
  theme_minimal()+
  xlab(NULL)+
  ylab("Wind Speed (km/h)")+
  theme(axis.text.x = element_text(angle = 90, hjust=1), legend.position=c(.20,.75))

ggplot(darroppt, aes(x=date, y=ppt))+
  #geom_line(size = 1, col = "darkblue")+
  geom_point()+
  theme_minimal()+
  xlab(NULL)+
  ylab("mm")+
  theme(axis.text.x = element_text(angle = 90, hjust=1), legend.position=c(.20,.75))

#Join data =====================================================================
df = full_join(torre, darroppt, by = "date")

df$id = c(1:length(df$date))
df$date = as.numeric(substr(df$date, 1, 4))

a = ggplot(df, aes(x=ppt, y=wind))+
  geom_point(aes(col = date), size = 2)+
  geom_smooth(method = "lm", col = "royalblue")+
  stat_cor(show.legend = F)+
  labs( x = "Daily accumulated precipitation (mm)", y = "Maximum Wind Speed per day (km/h)",
        title = "Precipitation (Darro Station) vs Wind Speed (Tower)")+
  scale_color_viridis()+
  theme_bw()

a

b = ggplot(df, aes(x=ppt, y=wind))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Daily accumulated precipitation (mm)", y = "Maximum Wind Speed per day (km/h)")+
  facet_wrap(~date)+
  scale_color_viridis()+
  theme_bw()

b

#ggsave(filename = "WD_tower-Prec_darro_allyears.png", plot = a,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 15, height = 12, units = "cm", dpi = 300)

#ggsave(filename = "WD_tower-Prec_darro_facet_allyears.png", plot = b,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 20, height = 12, units = "cm", dpi = 300)

#Remove years that do not exist data
df = df %>% filter(date %in% c(2014,2014,2016,2017,2018,2019,2020))

wd1420 = ggplot(df, aes(x=ppt, y=wind))+
  geom_point(aes(col = date), alpha = 0.7, size = 2)+
  geom_smooth(method = "lm", col = "royalblue")+
  stat_cor(show.legend = F)+
  labs( x = "Daily accumulated precipitation (mm)", y = "Maximum Wind Speed per day (km/h)",
        title = "Precipitation (Darro Station) vs Wind Speed (Tower)")+
  scale_color_viridis()+
  theme_bw()

wd1420

wd1420f = ggplot(df, aes(x=ppt, y=wind))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Daily accumulated precipitation (mm)", y = "Maximum Wind Speed per day (km/h)")+
  facet_wrap(~date, scales = "free")+
  scale_color_viridis()+
  theme_bw()

wd1420f


#ggsave(filename = "WD_tower-Prec_darro.png", plot = wd1420,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 15, height = 12, units = "cm", dpi = 300)

#ggsave(filename = "WD_tower-Prec_darro_facet.png", plot = wd1420f,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 20, height = 12, units = "cm", dpi = 300)












