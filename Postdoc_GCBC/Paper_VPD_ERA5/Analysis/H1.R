# H1 - The ERA5 performance along the successional gradient

#Eduardo Q Marques 19-05-2026

library(tidyverse)

#Load data ---------------------------------------------------------------------
setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/In situ/Kestrel_and_Hobos")
dir()
master = read.csv("Master_Kestrel_Hobo_VPD_16_02_2026.csv")
era = read.csv("ERA5_CP_SecFor_VPD_2026-04-27_to_2026-05-18.csv")

head(master)
head(era)

master$Date2 = substr(master$Date, 1, 13)
#master$Hour = substr(master$Date, 12, 13)

era$Date2 = substr(era$datetime, 1, 13)
#era$Hour = substr(era$datetime, 12, 13)

master2 = master %>% 
  group_by(Date2, Sample, Age) %>% 
  summarise(Temp_C = mean(Temp_C),
            RH = mean(RH),
            VPD = mean(VPD)) %>% 
  unite("id", Date2:Sample, remove = F)

colnames(era)[3:5] = c("Temp_ERA", "RH_ERA", "VPD_ERA")
era = era %>% unite("id", Date2, Sample, remove = F)



df = full_join(master2, era, by = "id")

df2 = df[,c(8,9,4,5,10,6,11,7,12)]









