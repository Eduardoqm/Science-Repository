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
nc = read.csv("NovaColonia_3104608_Nov_13_2026_6_10_00_PM.csv"  , sep = ",", skip = 3)
ncb = read.csv("NewColony_2-_3104596_Nov_13_2026_6_10_00_PM.csv"  , sep = ",", skip = 3)
ufra_k = read.csv("D2-3104592-CAP_UFRA_Nov_13_2026_9_20_00_AM.csv"  , sep = ",", skip = 3)
duq_k = read.csv("D2-3104605-S_GERALDO_27_de_ago_de_2026___2_10_00_PM.csv" , sep = ",", skip = 3)

#HOBO --------------------------------------------------------------------------
#Secundary Forests
ufra_h = read_excel("CAP_UFRA_22475754 2026-11-13 09_21_19 Brazil Standard Time (Data Brazil Standard Time).xlsx")
duq_h = read_excel("HOBO_SAO_GRD_22475752 2026-10-19 16_12_23 BRT (Data BRT).xlsx")
#romano = read_excel("Irituia.xlsx")

#SEDAP
pri = read_excel("PRIMARIA_10_09_2026.xlsx")
sdp_sec = read_excel("HOBO_SEDAP_SECUNDARIA_20_10_2026.xlsx")
pasto = read_excel("HOBO_SEDAP_PASTO_20_10_2026.xlsx")

#Processing kestrel dataframe --------------------------------------------------
nc2 = prcs_kestrel(nc, 8, "Nova_Colonia", "kestrel_d2")
ncb2 = prcs_kestrel(ncb, 8, "Nova_Colonia_B", "kestrel_d2"); ncb2 = ncb2 %>% filter(Date > "2026-10-15 00:00:00")
ufra_k2 = prcs_kestrel(ufra_k, 20, "SecFor_UFRA", "kestrel_d2")
duq_k2 = prcs_kestrel(duq_k, 32, "SecFor_Duquinha", "kestrel_d2")

#Filter current process data and merge
objs <- c("nc2", "ncb2", "ufra_k2", "duq_k2")
objs <- objs[sapply(objs, exists)]
kestrel <- do.call(rbind, mget(objs))

kestrel = kestrel %>% filter(Date > "2026-04-26 00:00:00") #Removing test time

#Convert Fahrenheit to Celsius
kestrel$Temp_C = ifelse(kestrel$Temp_C > 60, ((kestrel$Temp_C - 32) * 5/9), kestrel$Temp_C)

#Calculating VPD
kestrel$VPD = vpd(kestrel$Temp_C, kestrel$RH)
kestrel = kestrel[,c(1:3, 7, 4:6)]; head(kestrel)

#Processing HOBO dataframe -----------------------------------------------------
pri2 = prcs_hobo(pri, 100, "Primaria_SEDAP", "HOBO")
sdp_sec2 = prcs_hobo(sdp_sec, 15, "Secundaria_SEDAP", "HOBO")
pasto2 = prcs_hobo(pasto, 0, "Pasto_SEDAP", "HOBO")
ufra_h2 = prcs_hobo(ufra_h,  20, "SecFor_UFRA", "HOBO")
duq_h2 = prcs_hobo(duq_h, 32,  "SecFor_Duquinha", "HOBO")

#Filter current process data and merge
objs <- c("pri2", "sdp_sec2", "pasto2", "duq_h2", "ufra_h2")
objs <- objs[sapply(objs, exists)]
hobo <- do.call(rbind, mget(objs)); head(hobo)

#Mastering ---------------------------------------------------------------------
new_master = rbind(kestrel, hobo); new_master
#new_master = kestrel
#new_master = hobo

setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Kestrel_and_Hobos")
old_master = read_csv("Master_Hobo_Kestrel_Not_Calibrated_11_11_2026.csv"); old_master

master_final = bind_rows(old_master, new_master) %>% distinct(); master_final

#Exploration Graphs-------------------------------------------------------------
ggplot(master_final, aes(x = Date, y = Temp_C, col = Sensor))+
  geom_smooth()+
  facet_wrap(~Sample, scale = "free")

ggplot(master_final, aes(x = Date, y = RH, col = Sensor))+
  geom_smooth()+
  facet_wrap(~Sample, scale = "free")

ggplot(master_final, aes(x = Date, y = VPD, col = Sensor))+
  geom_smooth()+
  facet_wrap(~Sample, scale = "free")

ggplot(master_final, aes(x = Date, y = VPD, col = Sample))+
  geom_smooth()+
  geom_hline(yintercept = 0.75, linetype = "dashed")

ggplot(master_final, aes(x = Sample, y = VPD, fill = Sample))+
  geom_boxplot()+
  geom_hline(yintercept = 0.75, linetype = "dashed")

ggplot(master_final, aes(x = Age, y = VPD, fill = Sample))+
  geom_boxplot()+
  geom_hline(yintercept = 0.75, linetype = "dashed")

ggplot(master_final, aes(x = Sensor, y = VPD, fill = Sensor))+
  geom_boxplot()+
  geom_hline(yintercept = 0.75, linetype = "dashed")

ggplot(master_final, aes(x = Age, y = VPD))+
  geom_point()+
  geom_smooth(method = "lm")

summary(lm(VPD~Age, data = master_final))

#Exporting Master --------------------------------------------------------------
write.csv(master_final, "Master_Hobo_Kestrel_Not_Calibrated_16_11_2026.csv", row.names = F)


