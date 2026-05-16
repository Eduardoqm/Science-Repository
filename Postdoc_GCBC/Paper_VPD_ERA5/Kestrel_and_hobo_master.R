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


#Processing dataframe
prcs_kestrel = function(bd, age, sample){
  bd = bd[-1,c(1:3)]
  colnames(bd) = c("Date", "Temp_C", "RH")
  bd$age = age; bd$sample = sample
  
  bd$Date = as.POSIXct(bd$Date, format = "%Y-%m-%d %H:%M")
  bd$Temp_C = as.numeric(gsub(",", ".", bd$Temp_C))
  bd$RH = as.numeric(gsub(",", ".", bd$RH))
  return(bd)
}

ufra2 = prcs_kestrel(ufra, 20, "UFRA")
duquinha2 = prcs_kestrel(duquinha, 32, "Reserva S.Geraldo")
sdp_road2 = prcs_kestrel(sdp_road, 18, "Estrada SEDAP")

ufra = ufra[-1,c(1:3)]
colnames(ufra) = c("Date", "Temp_C", "RH")
ufra$age = 20; ufra$sample = "UFRA"


ufra$Date = as.POSIXct(ufra$Date, format = "%Y-%m-%d %H:%M")
ufra$Temp_C = as.numeric(gsub(",", ".", ufra$Temp_C))
ufra$RH = as.numeric(gsub(",", ".", ufra$RH))

head(ufra)












ggplot(ufra, aes(x = Date, y = Temp_C))+
  geom_point(size = 1, alpha = 0.5)+
  geom_smooth()

ggplot(ufra, aes(x = Date, y = RH))+
  geom_point(size = 1, alpha = 0.5)+
  geom_smooth()
