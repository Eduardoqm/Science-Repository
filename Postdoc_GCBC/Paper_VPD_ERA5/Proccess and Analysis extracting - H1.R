#Proccess and Analysis extracting - H1
#Eduardo Q Marques 03-10-2026

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
vpd_zone = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/Amazon_cons_VPD_Zone.tif")
vpd_zone
plot(vpd_zone)

#Mapbiomas
mb = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Brazil_Coverage_col11_2025.tif")
mb2 = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MapBiomas_Forest_2024_col10.tiff")
mb3 = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/MB_sum_pixel_Forest_2024_resampled.tiff")
plot(mb)
plot(mb2)
plot(mb3)

scf = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2024B.tif")
plot(scf)

#Amazonia limits
am = vect("G:/My Drive/Geodata/Vectors/Amazonia.shp")
plot(am, add = T)

#Clipping to Amazonia Biome ----------------------------------------------------
cons_vpd = mask(crop(cons_vpd, am), am)#; plot(cons_vpd)
vpd_95 = mask(crop(vpd_95, am), am)#; plot(vpd_95)
vpd_mean = mask(crop(vpd_mean, am), am)#; plot(vpd_mean)
vpd_zone = mask(crop(vpd_zone, am), am)#; plot(vpd_zone)

mbx = mask(crop(mb, am), am); plot(mbx)


#Filtering Forest class --------------------------------------------------------
mb2 = ifel(mbx == 3, 1, 0); plot(mb2) #Filter only forest
mb2 = ifel(scf > 0, 0, mb2); plot(mb2) #Remove Secondary Forests
setwd("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil")
writeRaster(mb2, "MapBiomas_Forest_2025_col11.tiff", overwrite=TRUE)

#Resampling rasters ------------------------------------------------------------
mb3 = resample(mb2, cons_vpd, method = "sum"); plot(mb3)
vpd_zone2 = resample(vpd_zone, cons_vpd, method = "average"); plot(vpd_zone2)

setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters")
writeRaster(mb3, "MB_sum_pixel_Forest_2025_resampled.tiff", overwrite = T)

#Time series -------------------------------------------------------------------
vpd_zone_df = as.data.frame(vpd_zone, na.rm= F)
for_df = as.data.frame(mb3, na.rm= F)

#Consecutive days in flammability conditions
df = as.data.frame(cons_vpd[[1]], na.rm = F)
colnames(df) = "cons_vpd"
df$year = names(cons_vpd[[1]])
df$zone = vpd_zone_df$mean
df$forest_p = for_df$classification_2024

for (z in 2:51) {
  print(names(cons_vpd[[z]]))
  df2 = as.data.frame(cons_vpd[[z]], na.rm = F)
  colnames(df2) = "cons_vpd"
  df2$year = names(cons_vpd[[z]])
  df2$zone = vpd_zone_df$mean
  df2$forest_p = for_df$classification_2024
  df = rbind(df, df2)
}

df2 = df |>
  filter(forest_p > 0) |> 
  na.omit()

ggplot(df2, aes(x=year, y=cons_vpd, fill = zone))+
  geom_boxplot()

df2$year = as.numeric(df2$year)
df2$zone = round(df2$zone, 0)
df2$zone2 = as.character(df2$zone)

df3 = df2 |> 
  group_by(year, zone2) |> 
  summarize(cons_vpd = mean(cons_vpd))

ggplot(df3, aes(x=year, y=cons_vpd))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  labs(x = NULL,
       y = "Number of days/year (VPD ≥ 0.75)")

ggplot(df3, aes(x=year, y=cons_vpd, col = zone2))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  labs(x = NULL,
       y = "Number of days/year (VPD ≥ 0.75)",
       col = "Dry Season
   (Months)")+
  theme_minimal()


#Annual VPD mean
df = as.data.frame(vpd_mean[[1]], na.rm = F)
colnames(df) = "vpd_mean"
df$year = names(vpd_mean[[1]])
df$zone = vpd_zone_df$mean
df$forest_p = for_df$classification_2024

for (z in 2:51) {
  print(names(vpd_mean[[z]]))
  df2 = as.data.frame(vpd_mean[[z]], na.rm = F)
  colnames(df2) = "vpd_mean"
  df2$year = names(vpd_mean[[z]])
  df2$zone = vpd_zone_df$mean
  df2$forest_p = for_df$classification_2024
  df = rbind(df, df2)
}

df2 = df |>
  filter(forest_p > 0) |> 
  na.omit()

ggplot(df2, aes(x=year, y=vpd_mean, fill = zone))+
  geom_boxplot()

df2$year = as.numeric(df2$year)
df2$zone = round(df2$zone, 0)
df2$zone2 = as.character(df2$zone)

df3 = df2 |> 
  group_by(year, zone2) |> 
  summarize(vpd_mean = mean(vpd_mean))

ggplot(df3, aes(x=year, y=vpd_mean))+
  geom_point()+
  geom_smooth(method = "lm", se = F)


ggplot(df3, aes(x=year, y=vpd_mean, col = zone2))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  labs(x = NULL,
       y = "Mean VPD/year",
       col = "Dry Season
   (Months)")+
  theme_minimal()

#Annual VPD quantile 95 (extremes)
df = as.data.frame(vpd_95[[1]], na.rm = F)
colnames(df) = "vpd_95"
df$year = names(vpd_95[[1]])
df$zone = vpd_zone_df$mean
df$forest_p = for_df$classification_2024

for (z in 2:51) {
  print(names(vpd_95[[z]]))
  df2 = as.data.frame(vpd_95[[z]], na.rm = F)
  colnames(df2) = "vpd_95"
  df2$year = names(vpd_95[[z]])
  df2$zone = vpd_zone_df$mean
  df2$forest_p = for_df$classification_2024
  df = rbind(df, df2)
}

df2 = df |>
  filter(forest_p > 0) |> 
  na.omit()

ggplot(df, aes(x=year, y=vpd_95, fill = zone))+
  geom_boxplot()

df2$year = as.numeric(df2$year)
df2$zone = round(df2$zone, 0)
df2$zone2 = as.character(df2$zone)

df3 = df2 |> 
  group_by(year, zone2) |> 
  summarize(vpd_95 = mean(vpd_95))

ggplot(df3, aes(x=year, y=vpd_95))+
  geom_point()+
  geom_smooth(method = "lm", se = F)


ggplot(df3, aes(x=year, y=vpd_95, col = zone2))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  labs(x = NULL,
       y = "VPD/year (quantile 95%)",
       col = "Dry Season
   (Months)")+
  theme_minimal()






