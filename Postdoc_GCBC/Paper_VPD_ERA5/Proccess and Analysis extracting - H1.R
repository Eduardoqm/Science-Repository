#Proccess and Analysis extracting - H1
#Eduardo Q Marques 

library(terra)
library(tidyverse)

#Functions ---------------------------------------------------------------------
brick = function(w, a, b){
  setwd(w)
  print(dir())
  list_rst = list.files()
  z = rast(list_rst)
  names(z) = substr(list_rst, a, b)
  #plot(z)
  return(z)
}

#Load data ---------------------------------------------------------------------
#VPD rasters
cons_vpd = brick("G:/My Drive/GEE_Max_Consecutive_VPD_Days_DP_Annual", 29, 32) #Consecutive VPD days > 0.75 kPa
vpd_95 = brick("G:/My Drive/GEE_Q95_VPD_DP_Annual", 12, 15) #Annual quantile 95 VDP
vpd_mean = brick("G:/My Drive/GEE_Mean_VPD_DP_Annual", 13, 16) #Annual mean VDP

#Driest Period length
dpm = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/DP_length_months.tif")
plot(dpm)

#Mapbiomas
mb = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MapBiomas_2024_col10.tiff")
fire_freq = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Fire_frequency_1985_2025.tiff")
plot(mb)
plot(fire_freq)

#Amazonia limits
am = vect("G:/My Drive/Geodata/Vectors/Amazonia.shp")
plot(am, add = T)

#Clipping to Amazonia Biome ----------------------------------------------------
cons_vpd = mask(crop(cons_vpd, am), am)#; plot(cons_vpd)
vpd_95 = mask(crop(vpd_95, am), am)#; plot(vpd_95)
vpd_mean = mask(crop(vpd_mean, am), am)#; plot(vpd_mean)
dpm = mask(crop(dpm, am), am)#; plot(dpm)

#Filtering Forest class --------------------------------------------------------
mb2 = ifel(mb == 3, mb, 0); plo(mb2) #Filter only forest

setwd("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil")
writeRaster(mb2, "MapBiomas_Forest_2024_col10.tiff")

mb3 = resample(mb2, cons_vpd, method = "sum"); plot(mb3)

dpm2 = resample(dpm, cons_vpd, method = "average"); plot(dpm2)

fire_freq2 = resample(fire_freq, cons_vpd, method = "average"); plot(fire_freq2)

#Time series -------------------------------------------------------------------

dpm_df = as.data.frame(dpm2, na.rm= F)
for_df = as.data.frame(mb3, na.rm= F)
fire_df = as.data.frame(fire_freq2, na.rm= F)

df = as.data.frame(cons_vpd[[1]], na.rm = F)
colnames(df) = "cons_vpd"
df$year = names(cons_vpd[[1]])
df$n_months = dpm_df$last
df$forest_p = for_df$xxxx
df$fire_freq = fire_df$xxxx

for (z in 2:51) {
  print(names(cons_vpd[[z]]))
  df2 = as.data.frame(cons_vpd[[z]], na.rm = F)
  colnames(df2) = "cons_vpd"
  df2$year = names(cons_vpd[[z]])
  df2$n_months = dpm_df$last
  df2$forest_p = for_df$xxxx
  df2$fire_freq = fire_df$xxxx
  df = rbind(df, df2)
}

df2 = df |>
#  filter(forest_p > 0) |> 
  na.omit()

ggplot(df2, aes(x=year, y=cons_vpd))+
  geom_boxplot()


df2$year = as.numeric(df2$year)
df2$n_months = round(df2$n_months, 0)
df2$n_months2 = as.character(df2$n_months)

df3 = df2 |> 
  group_by(year, n_months2) |> 
  summarize(cons_vpd = mean(cons_vpd);
            fire_freq = mean(fire_freq))

ggplot(df3, aes(x=year, y=cons_vpd))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~n_months2)






