#Kestrel and hobo Master dataframe - V2

#Eduardo Q Marques 10-09-2026

library(tidyverse)
library(readxl)

#Functions ---------------------------------------------------------------------
#Processing kestrel dataframe
prcs_kestrel = function(bd, age, sample, sensor){
  bd = bd[-1,c(1:3)]
  colnames(bd) = c("Date", "Temp_C", "RH")
  bd$Age = age; bd$Sample = sample; bd$Sensor = sensor
  
  bd$Date = as.POSIXct(bd$Date, format = "%Y-%m-%d %I:%M:%S %p")
  bd$Temp_C = as.numeric(gsub(",", ".", bd$Temp_C))
  bd$RH = as.numeric(gsub(",", ".", bd$RH))
  return(bd)
}

#Processing HOBO dataframe
prcs_hobo = function(bd, age, sample, sensor){
  bd = bd[,c(2,3,4,8)]
  colnames(bd) = c("Date", "Temp_C", "RH", "VPD")
  bd$Age = age; bd$Sample = sample; bd$Sensor = sensor
  bd$Date = as.POSIXct(bd$Date, format = "%Y-%m-%d %I:%M:%S %p")
  return(bd)
}

#Calculating VPD
vpd <- function(temp, UR) {
  es <- 0.6108 * exp((17.27 * temp) / (temp + 237.3)) #pressao de saturacao
  ea <- es * (UR / 100)                               #pressao real
  VPD <- es - ea                                      #deficit de pressao de vapor
  return(VPD)
}

#Load data ---------------------------------------------------------------------
setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Kestrel_and_Hobos/Raw_Data")
dir()

#Kestrel d2 --------------------------------------------------------------------
nc = read.csv("NovaColonia_3104608_4_de_ago._de_2026___9_30_00_AM.csv"  , sep = ",", skip = 3)
ufra_k = read.csv("D2-3104592-CAP_UFRA_4_de_ago._de_2026___5_00_00_PM.csv"  , sep = ",", skip = 3)
duq_k = read.csv("D2-3104605-S_GERALDO_27_de_ago_de_2026___2_10_00_PM.csv" , sep = ",", skip = 3)
romano = read.csv("IrituiaJRI_-_3104596_28_de_ago._de_2026___4_10_00_PM.csv", sep = ",", skip = 3)

#HOBO --------------------------------------------------------------------------
#Secundary Forests

#SEDAP
#pri = read_excel("PRIMARIA_10_09_2026.xlsx")
#sdp_sec = read_excel("SECUNDARIA_10_09_2026.xlsx")
#pasto = read_excel("PASTO_10_09_2026.xlsx" )

#Processing kestrel dataframe --------------------------------------------------
nc2 = prcs_kestrel(nc, 8, "Nova_Colonia", "kestrel_d2")
ufra_k2 = prcs_kestrel(ufra_k, 20, "SecFor_UFRA", "kestrel_d2")
duq_k2 = prcs_kestrel(duq_k, 32, "SecFor_Duquinha", "kestrel_d2")
romano2 = prcs_kestrel(romano, 87, "Prof_Romano", "kestrel_d2")

kestrel = rbind(nc2, ufra_k2, duq_k2, romano2)
kestrel = kestrel %>% filter(Date > "2026-04-26 00:00:00") #Removing test time

#Convert Fahrenheit to Celsius
kestrel$Temp_C = ifelse(kestrel$Temp_C > 60, ((kestrel$Temp_C - 32) * 5/9), kestrel$Temp_C)

#Calculating VPD
kestrel$VPD = vpd(kestrel$Temp_C, kestrel$RH)
kestrel = kestrel[,c(1:3, 7, 4:6)]

#Processing HOBO dataframe -----------------------------------------------------
pri2 = prcs_hobo(pri, 100, "Primaria_SEDAP", "HOBO")
sdp_sec2 = prcs_hobo(sdp_sec, 15, "Secundaria_SEDAP", "HOBO")
pasto2 = prcs_hobo(pasto, 0, "Pasto_SEDAP", "HOBO")

hobo = rbind(pri2, sdp_sec2, pasto2)
#Mastering ---------------------------------------------------------------------
master = rbind(kestrel, hobo)

#Exporting Master --------------------------------------------------------------
setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Kestrel_and_Hobos")
#write.csv(master, "Master_Hobo_Kestrel_Not_Calibrated_Sep_2026.csv", row.names = F)

#Exploration Graphs-------------------------------------------------------------
ggplot(master, aes(x = Date, y = Temp_C, col = Sample))+
  #geom_point(size = 1, alpha = 0.5)+
  geom_smooth()

ggplot(master, aes(x = Date, y = RH, col = Sample))+
  #geom_point(size = 1, alpha = 0.5)+
  geom_smooth()

ggplot(master, aes(x = Date, y = VPD, col = Sample))+
  #geom_point(size = 1, alpha = 0.5)+
  geom_smooth()+
  facet_wrap(~Sample, scale = "free")

ggplot(master, aes(x = Date, y = VPD, col = Sample))+
  #geom_point(size = 1, alpha = 0.5)+
  geom_smooth()+
  geom_hline(yintercept = 0.75, linetype = "dashed")

ggplot(master, aes(x = Sample, y = VPD, fill = Sample))+
  geom_boxplot()

