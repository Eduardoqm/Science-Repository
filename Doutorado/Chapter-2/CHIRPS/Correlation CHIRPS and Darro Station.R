#--------------------------------------
# Correlation CHIRPS and Darro Station
#--------------------------------------
# Eduardo Q Marques 23-03-2022
# eduardobio2009@gmail.com
#--------------------------------------

library(tidyverse)
library(reshape2)
library(ggplot2)
library(lubridate)
library(ggpubr)

#Load data ---------------------------------------------------------------------
setwd("~/Data/Tanguro Tower data")
chp19 = read.csv("CHIRPS_2019.csv", sep = ",")
darro = read.csv("Master_Estacao_Darro_2020.csv", sep = ",")

#Summarize CHIRPS by maximum precipitation--------------------------------------
chp19max = chp19 %>% 
  na.omit() %>% 
  group_by(date) %>% 
  summarise(prec = max(prec))

#Summarize CHIRPS by Darro Station location ------------------------------------
chp19$x = as.character(chp19$x)
chp19$y = as.character(chp19$y)

#chp19$x = substr(chp19$x, 1, 9)
#chp19$y = substr(chp19$y, 1, 9)

chp_darro = chp19 %>% 
  #unite("xy", x:y, sep = "_")  
  filter(x %in% c("-52.3830849495", "-52.38308")) %>% 
  filter(y %in% c("-13.0505040039012", "-13.0505")) %>% 
  select(date, prec)



#Summarize DARRO precipitation by accumulated precipitation per day-------------
darro19 = darro %>% 
  select(year, Date, ppt) %>% 
  filter(year == 2019) %>% 
  group_by(Date) %>% 
  summarise(ppt = sum(ppt))

#Transform date in julian date
darro19$Date = as.Date(darro19$Date)
darro19$Date = yday(darro19$Date)

#Join data frames --------------------------------------------------------------
colnames(darro19) = c("date", "Darro")
colnames(chp19max) = c("date", "CHIRPS")
colnames(chp_darro) = c("date", "CHIRPS")

prec19 = full_join(chp19max, darro19, by = "date")
prec19b = full_join(chp_darro, darro19, by = "date")


#Plot results ------------------------------------------------------------------
a = ggplot(prec19, aes(x = date))+
  geom_line(aes(y = Darro), col = "red", size = 1, alpha = 0.5)+
  geom_line(aes(y = CHIRPS), col = "blue", size = 1, alpha = 0.5)+
  labs(x = "Julian Days (2019)", y = "mm",
       title = "CHIRPS maximum value of buffer")+
  theme_minimal()

b = ggplot(prec19, aes(x=Darro, y=CHIRPS, col = date))+
  geom_point(alpha = 0.7)+
  geom_smooth(method = "lm")+
  stat_cor(show.legend = F)+
  theme_minimal()

ggarrange(a,b)


c = ggplot(prec19b, aes(x = date))+
  geom_line(aes(y = Darro), col = "red", size = 1, alpha = 0.5)+
  geom_line(aes(y = CHIRPS), col = "blue", size = 1, alpha = 0.5)+
  labs(x = "Julian Days (2019)", y = "mm",
       title = "CHIRPS pixel from Darro Station")+
  theme_minimal()

d = ggplot(prec19b, aes(x=Darro, y=CHIRPS, col = date))+
  geom_point(alpha = 0.7)+
  geom_smooth(method = "lm")+
  stat_cor(show.legend = F)+
  theme_minimal()

ggarrange(c,d)





  
