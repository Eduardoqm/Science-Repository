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
cons_vpd = mask(crop(cons_vpd, am), am); plot(cons_vpd)
vpd_95 = mask(crop(vpd_95, am), am); plot(vpd_95)
vpd_mean = mask(crop(vpd_mean, am), am); plot(vpd_mean)
dpm = mask(crop(dpm, am), am); plot(dpm)


#Time series -------------------------------------------------------------------
dpm2 = resample(dpm, cons_vpd, method = "average")
dpm_df = as.data.frame(dpm2, na.rm= F)


df = as.data.frame(cons_vpd[[1]], na.rm = F)
colnames(df) = "cons_vpd"
df$year = names(cons_vpd[[1]])
df$n_months = dpm_df$last

for (z in 2:51) {
  print(names(cons_vpd[[z]]))
  df2 = as.data.frame(cons_vpd[[z]], na.rm = F)
  colnames(df2) = "cons_vpd"
  df2$year = names(cons_vpd[[z]])
  df2$n_months = dpm_df$last
  df = rbind(df, df2)
}

df = df |>
  filter(cons_vpd > 0) |> 
  na.omit()

ggplot(df, aes(x=year, y=cons_vpd))+
  geom_boxplot()


df$year = as.numeric(df$year)

df2 = df |> 
  group_by(year) |> 
  summarize(cons_vpd = mean(cons_vpd),
            n_months, mean(n_months))

ggplot(df2, aes(x=year, y=cons_vpd))+
  geom_point()+
  geom_smooth()






