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


#Scenery 2 -> Converting Pasture in a 38 year old Forest -----------------------
#Scenery 2 and 3-> Converting Pasture in sf and persisting to 38 years ---------

#Input min, mean and maximun







#mean(3.27, 3.51, 3.30) #delta in 38 years old from the three years (2022 to 2024)
sf_lmar = -3.36 #Limiar value

past_lst = ifel(is.na(past), NA, lst_cur) #Filtering Pasture pixels

#Scenery 2
scn_sf = past_lst+sf_lmar
scn_sf2 = ifel(is.na(scn_sf), lst_cur, scn_sf)

mean(values(scn_sf2),na.rm=T) #32.84293

#Result
32.84 - 33.76 #-0.92
#Converting Pasture to 38 year old SF, the air temperature decrease 0.9°C.






































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









