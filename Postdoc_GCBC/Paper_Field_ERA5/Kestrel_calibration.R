#Kestrel D2 initial testes and comparisons

#Eduardo Q Marques 06-04-2026

library(tidyverse)

setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/Tests_Kestrel")
dir()

#Tests LAPEC -------------------------------------------------------------------
k1 = read.csv(dir()[1])
k2 = read.csv(dir()[2])
k3 = read.csv(dir()[3])
k4 = read.csv(dir()[4])

k1 = k1[,c(1:5)]
k2 = k2[,c(1:5)]
k3 = k3[,c(1:5)]
k4 = k4[,c(1:5)]


k1$sensor = "k1"
k2$sensor = "k2"
k3$sensor = "k3"
k4$sensor = "k4"

kdf = rbind(k1, k2, k3, k4)

kdf$Date = as.POSIXct(kdf$FORMATTED.DATE_TIME, format = "%m/%d/%Y %H:%M")

kdf2 = kdf %>% filter(Date > "2026-03-31 00:00:00")

kdf2$Temperature = as.numeric(gsub(",", ".", kdf2$Temperature))
kdf2$Relative.Humidity = as.numeric(gsub(",", ".", kdf2$Relative.Humidity))
kdf2$Heat.Index = as.numeric(gsub(",", ".", kdf2$Heat.Index))
kdf2$Dew.Point = as.numeric(gsub(",", ".", kdf2$Dew.Point))

head(kdf2)

ggplot(kdf2, aes(x = Date, y = Temperature, col = sensor))+
  geom_line(size = 3, alpha = 0.5)

ggplot(kdf2, aes(x = Date, y = Relative.Humidity, col = sensor))+
  geom_line(size = 3, alpha = 0.5)

ggplot(kdf2, aes(x = Date, y = Heat.Index, col = sensor))+
  geom_line(size = 3, alpha = 0.5)

ggplot(kdf2, aes(x = Date, y = Dew.Point, col = sensor))+
  geom_line(size = 3, alpha = 0.5)


#Test in the Pasture field -----------------------------------------------------
setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/Tests_Kestrel/Hobo_comparison")
dir()

kst = read.csv("Kestrel_D2_SEDAP_Pasto.csv")
hb = read.csv("HOBO_01 2026-04-14 09_45_13.csv", fileEncoding = "latin1")

head(kst)
head(hb)

kst$Date = as.POSIXct(kst$FORMATTED.DATE_TIME, format = "%m/%d/%Y %H:%M")
kst$Temperature = as.numeric(gsub(",", ".", kst$Temperature))
kst$Relative.Humidity = as.numeric(gsub(",", ".", kst$Relative.Humidity))
kst$Heat.Index = as.numeric(gsub(",", ".", kst$Heat.Index))
kst$Dew.Point = as.numeric(gsub(",", ".", kst$Dew.Point))
kst2 = kst[,c(14,2,3,5)]
colnames(kst2) = c("Date", "T_kestrel", "RH_kestrel", "DewPoint_kestrel")
kst2 = kst2 %>% filter(Date < "2026-04-14 09:45:00")


hb$Date = as.POSIXct(hb$Date.Time..Brazil.Standard.Time., format = "%m/%d/%Y %H:%M")
hb2 = hb[,c(9,2,3,8)]
colnames(hb2) = c("Date", "T_hobo", "RH_hobo", "DewPoint_hobo")
hb2 = hb2 %>% filter(Date > "2026-04-10 10:00:00")

df = full_join(kst2, hb2, by = "Date")
df = df %>% na.omit()

vpd <- function(temp, UR) {
  es <- 0.6108 * exp((17.27 * temp) / (temp + 237.3)) #pressao de saturacao
  ea <- es * (UR / 100)                               #pressao real
  VPD <- es - ea                                      #deficit de pressao de vapor
  return(VPD)
}

df$VPD_kestrel = vpd(df$T_kestrel, df$RH_kestrel)
df$VPD_hobo = vpd(df$T_hobo, df$RH_hobo)

summary(lm(VPD_hobo ~ VPD_kestrel, data = df))
summary(lm(T_hobo ~ T_kestrel, data = df))
summary(lm(RH_hobo ~ RH_kestrel, data = df))

#Calibration by LM slope
#Intercept+(Slope*Kestrel)
df$T_kestrel_C <- 4.462359 + (0.814337 * df$T_kestrel)
df$RH_kestrel_C <- 41.648026 + (0.572683 * df$RH_kestrel)

df$VPD_kestrel_C = vpd(df$T_kestrel_C, df$RH_kestrel_C)

summary(lm(VPD_hobo ~ VPD_kestrel_C, data = df))
summary(lm(T_hobo ~ T_kestrel_C, data = df))
summary(lm(RH_hobo ~ RH_kestrel_C, data = df))


ggplot(df, aes(x=T_hobo, y=T_kestrel))+
  geom_point()+
  geom_abline()+
  geom_smooth(method = "lm")

ggplot(df, aes(x=T_hobo, y=T_kestrel_C))+
  geom_point()+
  geom_abline()+
  geom_smooth(method = "lm")


ggplot(df, aes(x=RH_hobo, y=RH_kestrel))+
  geom_point()+
  geom_abline()+
  geom_smooth(method = "lm")

ggplot(df, aes(x=RH_hobo, y=RH_kestrel_C))+
  geom_point()+
  geom_abline()+
  geom_smooth(method = "lm")


ggplot(df, aes(x=VPD_hobo, y=VPD_kestrel))+
  geom_point()+
  geom_abline()+
  geom_smooth(method = "lm")

ggplot(df, aes(x=VPD_hobo, y=VPD_kestrel_C))+
  geom_point()+
  geom_abline()+
  geom_smooth(method = "lm")


