#Join full data

#Eduardo Q Marques 06-10-2025

library(ggplot2)
library(tidyverse)

#Load Data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Dados")
dir()

#Secondary Forest - Primary Forest
lst_a = read.csv("LST_SecFor_Age_Annual_full.csv")
lst_b = read.csv("LST_SecFor_Age_Dry_full.csv")
lst_c = read.csv("LST_SecFor_Age_Rainy_full.csv")

lst_pri = rbind(lst_a, lst_b, lst_c)
lst_pri$test = "Secondary - Primary Forest x SF age"

#Secondary Forest - Pasture
lst_a = read.csv("LST_Pasture_Annual_full.csv")
lst_b = read.csv("LST_Pasture_Dry_full.csv")
lst_c = read.csv("LST_Pasture_Rainy_full.csv")

lst_past = rbind(lst_a, lst_b, lst_c)
lst_past$test = "Secondary Forest - Pasture x SF age"

lst_sf = rbind(lst_pri, lst_past)

#Secondary Forest - Primary Forest x AGB
lst_a = read.csv("LST_AGB_Annual_full.csv")
lst_b = read.csv("LST_AGB_Dry_full.csv")
lst_c = read.csv("LST_AGB_Rainy_full.csv")

lst_pri_agb = rbind(lst_a, lst_b, lst_c)
lst_pri_agb$test = "Secondary - Primary Forest x AGB"

#Secondary Forest - Pasture x AGB
lst_a = read.csv("LST_AGB_Past_Annual_full.csv")
lst_b = read.csv("LST_AGB_Past_Dry_full.csv")
lst_c = read.csv("LST_AGB_Past_Rainy_full.csv")

lst_past_agb = rbind(lst_a, lst_b, lst_c)
lst_past_agb$test = "Secondary Forest - Pasture x AGB"

lst_agb = rbind(lst_pri_agb, lst_past_agb)

lst_agb$grupo = cut(lst_agb$agb, breaks = seq(0,10000, by = 5), labels = F)


#Evapotranpiration
#Secondary Forest - Primary Forest
et_a = read.csv("ET_SecFor_Age_Annual_full.csv")
et_b = read.csv("ET_SecFor_Age_Dry_full.csv")
et_c = read.csv("ET_SecFor_Age_Rainy_full.csv")

et_pri = rbind(et_a, et_b, et_c)
et_pri$test = "Secondary - Primary Forest x SF age"

#Secondary Forest - Pasture
et_a = read.csv("ET_Pasture_Annual_full.csv")
et_b = read.csv("ET_Pasture_Dry_full.csv")
et_c = read.csv("ET_Pasture_Rainy_full.csv")

et_past = rbind(et_a, et_b, et_c)
et_past$test = "Secondary Forest - Pasture x SF age"

et_sf = rbind(et_pri, et_past)

#Secondary Forest - Primary Forest x AGB
et_a = read.csv("ET_AGB_Annual_full.csv")
et_b = read.csv("ET_AGB_Dry_full.csv")
et_c = read.csv("ET_AGB_Rainy_full.csv")

et_pri_agb = rbind(et_a, et_b, et_c)
et_pri_agb$test = "Secondary - Primary Forest x AGB"

#Secondary Forest - Pasture x AGB
et_a = read.csv("ET_AGB_Past_Annual_full.csv")
et_b = read.csv("ET_AGB_Past_Dry_full.csv")
et_c = read.csv("ET_AGB_Past_Rainy_full.csv")

et_past_agb = rbind(et_a, et_b, et_c)
et_past_agb$test = "Secondary Forest - Pasture x AGB"

et_agb = rbind(et_pri_agb, et_past_agb)

et_agb$grupo = cut(et_agb$agb, breaks = seq(0,10000, by = 5), labels = F)



head(lst_sf)
head(lst_agb)
head(et_sf)
head(et_agb)


lst_sf2 = lst_sf %>% 
  filter(sf_perc >= 70)%>% 
  mutate(sf_age=round(sf_age,0)) %>% 
  group_by(delta_lst, sf_age, cond, test) %>% 
  summarise(delta_lst=mean(delta_lst,na.rm=T))

write.csv(lst_sf2, "LST_SecFor_full.csv", row.names = F)


lst_agb2 = lst_agb %>% 
  filter(sf_perc >= 70)%>% 
  filter(agb < 201) %>% 
  mutate(agb=round(agb,0))%>%
  group_by(delta_lst, agb, cond, grupo, test)%>%
  summarise(agb = mean(agb),delta_lst=mean(delta_lst,na.rm=T))

write.csv(lst_sf2, "LST_AGB_full.csv", row.names = F)


et_sf2 = et_sf %>% 
  filter(sf_perc >= 70)%>% 
  mutate(sf_age=round(sf_age,0))%>%
  group_by(test, sf_age, cond)%>%
  summarise(et=mean(delta_et,na.rm=T))

et_agb2 = et_agb %>% 
  filter(sf_perc >= 70)%>% 
  #filter(agb < 401) %>% 
  filter(agb < 201) %>% 
  mutate(agb=round(agb,0))%>%
  group_by(test, cond, grupo)%>%
  summarise(agb = mean(agb),et=mean(delta_et,na.rm=T))



































