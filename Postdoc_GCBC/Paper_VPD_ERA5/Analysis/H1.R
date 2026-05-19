# H1 - The ERA5 performance along the successional gradient

#Eduardo Q Marques 19-05-2026

library(tidyverse)

#Load data ---------------------------------------------------------------------
setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/In situ/Kestrel_and_Hobos")
dir()
master = read.csv("Master_Kestrel_Hobo_VPD_16_02_2026.csv")
era = read.csv("ERA5_CP_SecFor_VPD_2026-04-27_to_2026-05-18.csv")


#Organizing data ---------------------------------------------------------------
master$Date2 = substr(master$Date, 1, 13)
master2 = master %>% 
  group_by(Date2, Sample, Age) %>% 
  summarise(Temp_C = mean(Temp_C),
            RH = mean(RH),
            VPD = mean(VPD)) %>% 
  unite("id", Date2:Sample, remove = F)


era$Date2 = substr(era$datetime, 1, 13)
colnames(era)[3:5] = c("Temp_ERA", "RH_ERA", "VPD_ERA")
era = era %>% unite("id", Date2, Sample, remove = F)

#Joing Situ and ERA5 data ------------------------------------------------------
df = full_join(master2, era, by = "id")

df2 = df[,c(8,3,4,5,10,6,11,7,12)]
colnames(df2) = c("Date", "Sample", "Age", "Temp_situ", "Temp_ERA",
                  "RH_situ", "RH_ERA", "VPD_situ", "VPD_ERA")

df2$Date = as.POSIXct(df2$Date, format = "%Y-%m-%d %H:%M")

df3 = df2 %>% na.omit()

#Analysis ----------------------------------------------------------------------
summary(lm(df3$Temp_situ~df3$Temp_ERA))
summary(lm(df3$RH_situ~df3$RH_ERA))
summary(lm(df3$VPD_situ~df3$VPD_ERA))





#Exploratory Graphics ----------------------------------------------------------
ggplot(df3, aes(x = VPD_situ, y = VPD_ERA))+
  geom_point()+
  geom_smooth(method = lm)+
  facet_wrap(~Sample, scales = "free")


ggplot(df3, aes(x = Temp_situ, y = Temp_ERA))+
  geom_point()+
  geom_smooth(method = lm)+
  facet_wrap(~Sample, scales = "free")


ggplot(df3, aes(x = RH_situ, y = RH_ERA))+
  geom_point()+
  geom_smooth(method = lm)+
  facet_wrap(~Sample, scales = "free")










