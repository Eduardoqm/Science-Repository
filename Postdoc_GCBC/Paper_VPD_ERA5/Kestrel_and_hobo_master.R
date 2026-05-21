#Kestrel and hobo Master dataframe

#Eduardo Q Marques 16-05-2026

library(tidyverse)
library(readxl)

setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/In situ/Kestrel_and_Hobos/Raw_Data")
dir()

#Kestrel d2 --------------------------------------------------------------------
ufra = read.csv("D2-3104592-CAP_UFRA_12_de_mai._de_2026___4_20_00_PM.csv",
                sep = ",", skip = 3)  #Skip first line informations


duquinha = read.csv("D2-3104605-S_GERALDO_7_de_mai._de_2026___11_10_00_AM.csv",
                sep = ",", skip = 3)  #Skip first line informations


sdp_road = read.csv("D2_-_3104596-ESTRADA_7_de_mai._de_2026___10_20_00_AM.csv",
                sep = ",", skip = 3)  #Skip first line informations

pri = read_excel("HOBO_2026-05-05_PriFor_SEDAP.xlsx")

pasto = read_excel("HOBO_01 2026-05-07_Pasto_SEDAP.xlsx")

#Processing kestrel dataframe --------------------------------------------------
prcs_kestrel = function(bd, age, sample, sensor){
  bd = bd[-1,c(1:3)]
  colnames(bd) = c("Date", "Temp_C", "RH")
  bd$Age = age; bd$Sample = sample; bd$Sensor = sensor
  
  bd$Date = as.POSIXct(bd$Date, format = "%Y-%m-%d %I:%M:%S %p")
  bd$Temp_C = as.numeric(gsub(",", ".", bd$Temp_C))
  bd$RH = as.numeric(gsub(",", ".", bd$RH))
  return(bd)
}

sdp_road2 = prcs_kestrel(sdp_road, 18, "Estrada_SEDAP", "kestrel_d2")
ufra2 = prcs_kestrel(ufra, 20, "SecFor_UFRA", "kestrel_d2")
duquinha2 = prcs_kestrel(duquinha, 32, "SecFor_Duquinha", "kestrel_d2")
duquinha2 = duquinha2 %>% filter(Date > "2026-04-26 00:00:00")

kestrel = rbind(sdp_road2, ufra2, duquinha2)

#Calibration by HOBO LM slope
#Intercept+(Slope*Kestrel)
kestrel$Temp_C <- 4.462359 + (0.814337 * kestrel$Temp_C)
kestrel$RH <- 41.648026 + (0.572683 * kestrel$RH)

#Calculating VPD
vpd <- function(temp, UR) {
  es <- 0.6108 * exp((17.27 * temp) / (temp + 237.3)) #pressao de saturacao
  ea <- es * (UR / 100)                               #pressao real
  VPD <- es - ea                                      #deficit de pressao de vapor
  return(VPD)
}

kestrel$VPD = vpd(kestrel$Temp_C, kestrel$RH)
kestrel = kestrel[,c(1:3, 7, 4:6)]

#Processing HOBO dataframe -----------------------------------------------------
prcs_hobo = function(bd, age, sample, sensor){
  bd = bd[,c(2,3,4,8)]
  colnames(bd) = c("Date", "Temp_C", "RH", "VPD")
  bd$Age = age; bd$Sample = sample; bd$Sensor = sensor
  bd$Date = as.POSIXct(bd$Date, format = "%Y-%m-%d %I:%M:%S %p")
  return(bd)
}

pri2 = prcs_hobo(pri, 100, "Primaria_SEDAP", "HOBO")
pasto2 = prcs_hobo(pasto, 0, "Pastagem_SEDAP", "HOBO")

hobo = rbind(pri2, pasto2)
#Mastering ---------------------------------------------------------------------
master = rbind(kestrel, hobo)

#Exporting Master --------------------------------------------------------------
setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/In situ/Kestrel_and_Hobos")
write.csv(master, "Master_Kestrel_Hobo_VPD_16_02_2026.csv", row.names = F)

#Exploration Graphs-------------------------------------------------------------
ggplot(master, aes(x = Date, y = Temp_C, col = Sample))+
  geom_point(size = 1, alpha = 0.5)+
  geom_smooth()

ggplot(master, aes(x = Date, y = RH, col = Sample))+
  geom_point(size = 1, alpha = 0.5)+
  geom_smooth()

ggplot(master, aes(x = Date, y = VPD, col = Sample))+
  geom_point(size = 1, alpha = 0.5)+
  geom_smooth()

ggplot(master, aes(x = Sample, y = VPD, col = Sample))+
  geom_boxplot()

