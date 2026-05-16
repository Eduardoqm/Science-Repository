#Kestrel and hobo Master dataframe

#Eduardo Q Marques 16-05-2026

library(tidyverse)

setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/In situ/Kestrel_and_Hobos/Kestrel_raw")
dir()

#Kestrel d2 --------------------------------------------------------------------
ufra = read.csv("D2-3104592-CAP_UFRA_12_de_mai._de_2026___4_20_00_PM.csv",
                sep = ",", skip = 3)  #Skip first line informations


duquinha = read.csv("D2-3104605-S_GERALDO_7_de_mai._de_2026___11_10_00_AM.csv",
                sep = ",", skip = 3)  #Skip first line informations


sdp_road = read.csv("D2_-_3104596-ESTRADA_7_de_mai._de_2026___10_20_00_AM.csv",
                sep = ",", skip = 3)  #Skip first line informations


#Processing dataframe ----------------------------------------------------------
prcs_kestrel = function(bd, age, sample, sensor){
  bd = bd[-1,c(1:3)]
  colnames(bd) = c("Date", "Temp_C", "RH")
  bd$age = age; bd$sample = sample; bd$sensor = sensor
  
  bd$Date = as.POSIXct(bd$Date, format = "%Y-%m-%d %H:%M")
  bd$Temp_C = as.numeric(gsub(",", ".", bd$Temp_C))
  bd$RH = as.numeric(gsub(",", ".", bd$RH))
  return(bd)
}

sdp_road2 = prcs_kestrel(sdp_road, 18, "Estrada SEDAP", "kestrel_d2")
ufra2 = prcs_kestrel(ufra, 20, "UFRA", "kestrel_d2")
duquinha2 = prcs_kestrel(duquinha, 32, "Reserva S.Geraldo", "kestrel_d2")
duquinha2 = duquinha2 %>% filter(Date > "2026-04-26 00:00:00")


#Mastering ---------------------------------------------------------------------
master = rbind(sdp_road2, ufra2, duquinha2)

#Calibration by HOBO LM slope --------------------------------------------------
#Intercept+(Slope*Kestrel)
master$Temp_C <- 4.462359 + (0.814337 * master$Temp_C)
master$RH <- 41.648026 + (0.572683 * master$RH)

#Calculating VPD ---------------------------------------------------------------
vpd <- function(temp, UR) {
  es <- 0.6108 * exp((17.27 * temp) / (temp + 237.3)) #pressao de saturacao
  ea <- es * (UR / 100)                               #pressao real
  VPD <- es - ea                                      #deficit de pressao de vapor
  return(VPD)
}

master$VPD = vpd(master$Temp_C, master$RH)

#Exporting Master --------------------------------------------------------------

#Exploration Graphs-------------------------------------------------------------
ggplot(master, aes(x = Date, y = Temp_C, col = sample))+
  geom_point(size = 1, alpha = 0.5)+
  geom_smooth()

ggplot(master, aes(x = Date, y = RH, col = sample))+
  geom_point(size = 1, alpha = 0.5)+
  geom_smooth()

ggplot(master, aes(x = Date, y = VPD, col = sample))+
  geom_point(size = 1, alpha = 0.5)+
  geom_smooth()

ggplot(master, aes(x = sample, y = VPD, col = sample))+
  geom_boxplot()

