#Time Series Microclimate PurpleAir CP

#Eduardo Q Marques 05-11-2025

library(tidyverse)
library(ggpubr)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Research/PosDoc_GCBC/Analises/In situ") #Ecostation
#setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/In situ") #Laptop
dir()

pp = read_csv("PurpleAir_full_series_CP.csv", col_types = cols(.default = "c"))

#Ajusting table
pp$hour = (as.numeric(substr(pp$UTCDateTime, 12, 13)) - 3) #Adjust to BsB time -3 hours
pp$hour[pp$hour == -3] = 23
pp$hour[pp$hour == -2] = 22
pp$hour[pp$hour == -1] = 21

pp$minute = as.numeric(substr(pp$UTCDateTime, 15, 16))

pp <- sapply(pp, as.numeric )
pp = as.data.frame(pp[,-1])


#VPD ---------------------------------------------------------------------------
vpd <- function(temp, rh) {
  es <- 0.6108 * exp((17.27 * temp) / (temp + 237.3)) # Saturation Pressure
  ea <- es * (rh / 100)                               # Real Pressure
  VPD <- es - ea                                      # Vapor Pressure Deficit
  return(VPD)
}

pp$vpd = vpd(pp$temp_Celcius, pp$current_humidity)


vpd_dry = pp %>% 
  filter(month == c(5, 6, 7, 8, 9)) %>% 
  group_by(hour, month) %>% 
  summarise(vpd = mean(vpd))
vpd_dry$season = "Dry Season"

vpd_rainy = pp %>% 
  filter(month == c(10, 11, 12, 1, 2, 3, 4)) %>% 
  group_by(hour, month) %>% 
  summarise(vpd = mean(vpd))
vpd_rainy$season = "Rainy Season"

vpd_all = rbind(vpd_dry, vpd_rainy)



vpd_m = pp %>% 
  group_by(hour, month) %>% 
  summarise(vpd = mean(vpd))

ggplot(vpd_m, aes(x=hour, y=vpd, col=as.character(month)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(se = F)+
  labs(title = "a)", x = "Hour", y = "VPD (kPa)", col = "Months")
  #facet_grid(~month)


rh_m = pp %>% 
  group_by(hour, month) %>% 
  summarise(current_humidity = mean(current_humidity))

ggplot(rh_m, aes(x=hour, y=current_humidity, col=as.character(month)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(se = F)+
  labs(title = "b)", x = "Hour", y = "Relative Humidity (%)", col = "Months")


temp_m = pp %>% 
  group_by(hour, month) %>% 
  summarise(temp_Celcius = mean(temp_Celcius))

ggplot(temp_m, aes(x=hour, y=temp_Celcius, col=as.character(month)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(se = F)+
  labs(title = "c)", x = "Hour", y = "Temperature (ºC)", col = "Months")
