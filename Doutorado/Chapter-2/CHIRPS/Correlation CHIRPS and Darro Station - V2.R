#--------------------------------------
# Correlation CHIRPS and Darro Station
#--------------------------------------
# Eduardo Q Marques 28-03-2022
# eduardobio2009@gmail.com
#--------------------------------------

library(tidyverse)
library(reshape2)
library(viridis)
library(ggplot2)
library(lubridate)
library(ggpubr)

#Load data ---------------------------------------------------------------------
setwd("~/Data/Tanguro Tower data")
chp = read.csv("CHIRPS_2012-2020.csv", sep = ",")
darro = read.csv("Master_Estacao_Darro_2020.csv", sep = ",")

#Summarize CHIRPS by maximum precipitation--------------------------------------
chpmax = chp %>% 
  na.omit() %>% 
  group_by(year, date) %>% 
  summarise(prec = max(prec))

#Summarize CHIRPS by Darro Station location ------------------------------------
chp$x = as.character(chp$x)
chp$y = as.character(chp$y)

#chp19$x = substr(chp19$x, 1, 9)
#chp19$y = substr(chp19$y, 1, 9)

chp_darro = chp %>% 
  #unite("xy", x:y, sep = "_")  
  filter(x %in% c("-52.3830849495", "-52.38308")) %>% 
  filter(y %in% c("-13.0505040039012", "-13.0505")) %>% 
  select(year, date, prec)

#Summarize DARRO precipitation by accumulated precipitation per day-------------
darro = darro %>% 
  select(year, Date, ppt) %>% 
  #filter(year == 2019) %>% 
  group_by(year, Date) %>% 
  summarise(ppt = sum(ppt))

#Transform date in julian date
darro$Date = as.Date(darro$Date)
darro$Date = yday(darro$Date)

#Join data frames --------------------------------------------------------------
colnames(darro) = c("year", "date", "Darro")
colnames(chpmax) = c("year", "date", "CHIRPS")
colnames(chp_darro) = c("year", "date", "CHIRPS")

prec = full_join(chpmax, darro, by = c("date", "year"))
prec2 = full_join(chp_darro, darro, by = c("date", "year"))


#Plot results ------------------------------------------------------------------
a = ggplot(prec, aes(x = date))+
  geom_line(aes(y = Darro), col = "red", size = 1, alpha = 0.5)+
  geom_line(aes(y = CHIRPS), col = "blue", size = 1, alpha = 0.5)+
  labs(x = "Julian Days (2019)", y = "mm",
       title = "CHIRPS maximum value of buffer")+
  theme_minimal(); a

b = ggplot(prec, aes(x=Darro, y=CHIRPS, col = year))+
  geom_point(alpha = 0.7, size = 2)+
  geom_smooth(method = "lm")+
  stat_cor(show.legend = F)+
  labs(x = "Darro weather station (mm/day)", y = "CHIRPS (mm/day)",
       title = "CHIRPS maximum value of buffer")+
  scale_color_viridis()+
  theme_minimal(); b

ggarrange(a,b)


c = ggplot(prec2, aes(x = date))+
  geom_line(aes(y = Darro), col = "red", size = 1, alpha = 0.5)+
  geom_line(aes(y = CHIRPS), col = "blue", size = 1, alpha = 0.5)+
  labs(x = "Julian Days (2019)", y = "mm",
       title = "CHIRPS pixel from Darro Station")+
  theme_minimal(); c

d = ggplot(prec2, aes(x=Darro, y=CHIRPS, col = year))+
  geom_point(alpha = 0.7, size = 2)+
  geom_smooth(method = "lm")+
  stat_cor(show.legend = F)+
  labs(x = "Darro weather station (mm/day)", y = "CHIRPS (mm/day)",
       title = "CHIRPS pixel from Darro weather station")+
  scale_color_viridis()+
  theme_minimal(); d

ggarrange(c,d)

ggarrange(b,d)

ggsave(filename = "Darro_CHIRPS max buffer.png", plot = b,
      path = "~/Figures", width = 15, height = 12, units = "cm", dpi = 300)

ggsave(filename = "Darro_CHIRPS pixel station.png", plot = d,
       path = "~/Figures", width = 15, height = 12, units = "cm", dpi = 300)


