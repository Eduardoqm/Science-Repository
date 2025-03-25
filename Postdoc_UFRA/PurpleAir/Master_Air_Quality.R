#Master of PurpleAir Air Quality tables

#Created by Divino V. Silverio

library(tidyverse)

setwd("C:/Users/Workshop/Documents/Research/PurpleAir/CP_Data")
dir()

fl=dir(pattern = "*.csv$");fl


#dt= do.call(rbind,lapply(fl,read.csv))
#dt <- list.files(fl, full.names = TRUE) %>%
#  map_dfr(read_csv)

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

setwd("C:/Users/Workshop/Documents/Research/PurpleAir")
write.csv(dt2,"Master_qualidade_ar_cp_B.csv")


#ggplot(dt2,aes(time_bsb,temp_Celcius))+
#  geom_point()
