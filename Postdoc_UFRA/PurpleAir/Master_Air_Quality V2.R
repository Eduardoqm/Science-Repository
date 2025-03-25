#----------------------------------#
# Master of PurpleAir Air Quality  #
#                                  #
# Eduardo Q Marques 21-03-2025     #
#----------------------------------#

library(tidyverse)

#Load data ---------------------------------------------------------------------
master = read_csv("C:/Users/Eduardo/Documents/PurpleAir/Master_qualidade_ar_cp.csv",
                  col_types = cols(.default = "c"))

master = master[,-1]

#Data o be input on master -----------------------------------------------------
setwd("C:/Users/Eduardo/Documents/PurpleAir/CP_data")
dir()

fl=dir(pattern = "*.csv$");fl

dt=fl%>% 
  map_df(~read_csv(.,col_types  = cols(.default = "c")))
names(dt)

dt2=dt%>%
  mutate(
    st_nchar=nchar(hardware))%>%
  dplyr::filter(st_nchar>53)%>%
  mutate(
    temp_Celcius=round((as.numeric(current_temp_f) - 32) * (5/9),2),
    dewpoint_Celcius=round((as.numeric(current_dewpoint_f) - 32) * (5/9),2),
    time_bsb=with_tz(as_datetime(UTCDateTime, tz = "UTC"), tz="America/Buenos_Aires"),
    ano=lubridate::year(time_bsb),
    mes=lubridate::month(time_bsb),
    dia=lubridate::day(time_bsb),
    hora=hms::as_hms(time_bsb))%>%
  dplyr::filter(time_bsb>as.Date('2022-07-01'))

#Join data to Master -----------------------------------------------------------
master = rbind(master, dt2)
master2 = master[!duplicated(master$UTCDateTime), ]

#Saving the complete version of Master -----------------------------------------
setwd("C:/Users/Eduardo/Documents/PurpleAir")
write.csv(master2,"Master_qualidade_ar_cp_B.csv")

#Verifying data ----------------------------------------------------------------
master3$Date = as.Date(substr(master3$UTCDateTime, 1,10), "%Y/%m/%d")

ggplot(master3, aes(Date, temp_Celcius))+
  geom_point(alpha = 0.1)+
  theme_minimal()






















