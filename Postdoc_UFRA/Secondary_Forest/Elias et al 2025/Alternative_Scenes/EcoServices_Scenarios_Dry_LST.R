#Eco Services Scenarios (Dry) - LST
#Eduardo Q Marques 04-02-2026

library(terra)
library(tidyverse)

setwd("G:/Meu Drive/Dados_Elias_paper/LST_ET_scenario")
dir()

#Load data ---------------------------------------------------------------------
lst_cur = rast("LST_Dry_current.tif")
scf = rast("MB_Forest_age_70m.tif")
past = rast("Pasture_70m.tif")
prf = rast("Forest_70m.tif")
dlt_lst_p = rast("Pasture_Delta_LST_Dry.tif")
df_delta = read.csv("G:/Meu Drive/Dados_Elias_paper/Delta_Data_Frame/LST_Pasture_age_FULL.csv")

plot(lst_cur)
plot(dlt_lst_p)


#Scenery 1 -> Coverting SF in Pasture ------------------------------------------
dlt_lst_p2 = ifel(is.na(scf), NA, dlt_lst_p) #Filtering SF
plot(dlt_lst_p2)

dlt_lst_p3 = dlt_lst_p2*-1 #Inverting signal to make sense
plot(dlt_lst_p3)

#Scenery 1
scn_p = lst_cur+dlt_lst_p3
scn_p2 = ifel(is.na(scn_p), lst_cur, scn_p)

mean(values(scn_p2),na.rm=T) #33.81549
mean(values(lst_cur),na.rm=T) #33.76452

#Result
33.81 - 33.76 #0.05
#Converting SF in Pasture increase the air temperature in 0.05°C.


#Scenery 2 and 3-> Converting Pasture in sf and persisting to 38 years ---------
df_delta2 = df_delta %>% 
  filter(cond == "Dry Season") %>% 
  group_by(age) %>% 
  summarise(delta_lst = mean(delta_lst))

#Input min, mean and maximun
max_lmar = min(df_delta2$delta_lst) #Delta in 38 years (oldest SF)
mean_lmar = mean(df_delta2$delta_lst) #Mean delta
min_lmar = max(df_delta2$delta_lst) #Delta for youngster SF

past_lst = ifel(is.na(past), NA, lst_cur) #Filtering Pasture pixels

#Delta for youngster SF
scn_min_sf = past_lst+min_lmar
scn_min_sf2 = ifel(is.na(scn_min_sf), lst_cur, scn_min_sf)

mean(values(scn_min_sf2),na.rm=T) #33.6648

#Result
33.66 - 33.76 #-0.1
#Converting Pasture to young SF, the air temperature decrease 0.1°C.

#Mean delta
scn_mean_sf = past_lst+mean_lmar
scn_mean_sf2 = ifel(is.na(scn_mean_sf), lst_cur, scn_mean_sf)

mean(values(scn_mean_sf2),na.rm=T) #33.11481

#Result
33.11 - 33.76 #-0.65
#Converting Pasture to SF, in mean the air temperature decrease 0.65°C.


#Mean delta
scn_max_sf = past_lst+max_lmar
scn_max_sf2 = ifel(is.na(scn_max_sf), lst_cur, scn_max_sf)

mean(values(scn_max_sf2),na.rm=T) #32.8423

#Result
32.84 - 33.76 #-0.92
#Converting Pasture to 38 year old SF, the air temperature decrease 0.9°C.


#Saving rasters of scenarios ---------------------------------------------------
setwd("G:/Meu Drive/Dados_Elias_paper/LST_ET_scenario/Scenery_Raster")
writeRaster(scn_p2, "Scenary_LST_SF_to_Pasture.tif")
writeRaster(scn_min_sf2, "Scenary_LST_Pasture_to_young_SF.tif")
writeRaster(scn_mean_sf2, "Scenary_LST_Pasture_to_mean_SF.tif")
writeRaster(scn_max_sf2, "Scenary_LST_Pasture_to_old_SF.tif")
























#Scenery 3 -> Forest until 10 years holding 20 years without deforestation -----
scf_lst = ifel(is.na(scf), NA, lst_cur)

summary(values(scf_lst), na.rm = T)

dfa = as.data.frame(scf_lst)
dfb = as.data.frame(ifel(is.na(scf_lst), NA, scf))
df = cbind(dfa, dfb)

colnames(df) = c("Temp_C", "Age")
df$Age2 = round(df$Age, digits = 0)

df2 = df %>% 
  group_by(Age2) %>% 
  summarise(Temp_C = mean(Temp_C))

df2$Temp_C20 = 0

#Until 19 years old 
for (z in 1:19) {
  k = z+19
  df2[z,3] = df2[z,2]-df2[k,2]
}

#20 years more
for (z in 20:38) {
  df2[z,3] = df2[z,2]-df2[38,2]
}

write.csv(df2, "SF_LST_Mean_diff_20years.csv", row.names = F)

#Applying differences in SF LST
diff_list <- list(df2$Temp_C20)
scf2 = round(scf, digits = 0)

#scf_lst2 = ifel(scf2 == 1, scf_lst-diff_list[[1]][1], scf_lst)

for (z in 1:37) {
  print(z)
  scf_lst2 = ifel(scf2 == z, scf_lst-diff_list[[1]][z], scf_lst)
}

scn_sf3 = ifel(is.na(scf_lst2), lst_cur, scf_lst2)

mean(values(scn_sf3),na.rm=T) #33.7643

33.7643 - 33.76452 #-0.00022

#Persistence of young secondary forests for 20 years without deforestation decrease the air temperature in 0.00022°C.



mean(values(scf_lst2), na.rm = T)
mean(values(scf_lst), na.rm = T)


ggplot(df2, aes(x = Age2))+
  geom_point(aes(y = Temp_C), col = "blue")+
  geom_point(aes(y = Temp_C-Temp_C20))
  #geom_smooth()


ggplot(df2, aes(x = Age2, y = Temp_C20))+
  geom_point()+
  geom_smooth()









