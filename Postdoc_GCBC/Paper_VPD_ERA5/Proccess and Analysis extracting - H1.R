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
vpd_hour = brick("G:/My Drive/GEE_Annual_VPD_Hours_DP", 21, 24)

#Driest Period length
vpd_zone = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/Amazon_cons_VPD_Zone.tif")
vpd_zone
plot(vpd_zone)

dp_month = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/DP_length_months.tif") 

#Mapbiomas
#mb = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Brazil_Coverage_col11_2025.tif")
#mb2 = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MapBiomas_Forest_2024_col10.tiff")
mb3 = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/MB_sum_pixel_Forest_2024_resampled.tiff")
#plot(mb)
#plot(mb2)
plot(mb3)

#scf = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2024B.tif")
#plot(scf)

#Amazonia limits
am = vect("G:/My Drive/Geodata/Vectors/Amazonia.shp")
plot(am, add = T)

#Clipping to Amazonia Biome ----------------------------------------------------
cons_vpd = mask(crop(cons_vpd, am), am)
vpd_95 = mask(crop(vpd_95, am), am)
vpd_mean = mask(crop(vpd_mean, am), am)
vpd_hour = mask(crop(vpd_hour, am), am)
vpd_zone = mask(crop(vpd_zone, am), am)
dp_month = mask(crop(dp_month, am), am)
#mbx = mask(crop(mb, am), am); plot(mbx)

#Filtering Forest class --------------------------------------------------------
#mb2 = ifel(mbx == 3, 1, 0); plot(mb2) #Filter only forest
#scf = mask(crop(scf, am), am)
#mb2 = ifel(scf > 0, 0, mb2); plot(mb2) #Remove Secondary Forests
#setwd("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil")
#writeRaster(mb2, "MapBiomas_Forest_2025_col11.tiff", overwrite=TRUE)

#Resampling rasters ------------------------------------------------------------
vpd_zone = resample(vpd_zone, cons_vpd, method = "average"); plot(vpd_zone)
dp_month2 = resample(dp_month, cons_vpd, method = "average"); plot(dp_month2)
#mb3 = resample(mb2, cons_vpd, method = "sum"); plot(mb3)
#setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters")
#writeRaster(mb3, "MB_sum_pixel_Forest_2025_resampled.tiff", overwrite = T)

#Time series -------------------------------------------------------------------
vpd_zone_df = as.data.frame(vpd_zone, na.rm= F)
for_df = as.data.frame(mb3, na.rm= F)

#Consecutive days in flammability conditions VPD > 0.75 ------------------------
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

df2$year = as.numeric(df2$year)
df2$zone = round(df2$zone, 0)
df2$zone = as.character(df2$zone)

df3 = df2 |> 
  group_by(year, zone) |> 
  summarize(cons_vpd = mean(cons_vpd))

df3$var = "cons_vpd"
colnames(df3)[3] = "value"

ggplot(df3, aes(x=year, y=value, col = zone))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  labs(x = NULL,
       y = "Number of days/year (VPD ≥ 0.75)",
       col = NULL)+
  theme_minimal()

df_all = df3

#Annual VPD mean ---------------------------------------------------------------
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

df2$year = as.numeric(df2$year)
df2$zone = round(df2$zone, 0)
df2$zone = as.character(df2$zone)

df3 = df2 |> 
  group_by(year, zone) |> 
  summarize(vpd_mean = mean(vpd_mean))

df3$var = "vpd_mean"
colnames(df3)[3] = "value"

ggplot(df3, aes(x=year, y=value, col = zone))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  labs(x = NULL,
       y = "Mean VPD/year",
       col = NULL)+
  theme_minimal()

df_all = rbind(df_all, df3)

#Annual VPD quantile 95 (extremes) ---------------------------------------------
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

df2$year = as.numeric(df2$year)
df2$zone = round(df2$zone, 0)
df2$zone = as.character(df2$zone)

df3 = df2 |> 
  group_by(year, zone) |> 
  summarize(vpd_95 = mean(vpd_95))

df3$var = "vpd_95"
colnames(df3)[3] = "value"

ggplot(df3, aes(x=year, y=value, col = zone))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  labs(x = NULL,
       y = "VPD/year (quantile 95%)",
       col = NULL)+
  theme_minimal()

df_all = rbind(df_all, df3)

#DP hours/day in flammability conditions VPD > 0.75 ----------------------------
vpd_hour2 = (vpd_hour/dp_month2)/30 #Convert annual total hours in hours/day

df = as.data.frame(vpd_hour2[[1]], na.rm = F)
colnames(df) = "vpd_hour2"
df$year = names(vpd_hour2[[1]])
df$zone = vpd_zone_df$mean
df$forest_p = for_df$classification_2024

for (z in 2:51) {
  print(names(vpd_hour2[[z]]))
  df2 = as.data.frame(vpd_hour2[[z]], na.rm = F)
  colnames(df2) = "vpd_hour2"
  df2$year = names(vpd_hour2[[z]])
  df2$zone = vpd_zone_df$mean
  df2$forest_p = for_df$classification_2024
  df = rbind(df, df2)
}

df2 = df |>
  filter(forest_p > 0) |> 
  na.omit()

df2$year = as.numeric(df2$year)
df2$zone = round(df2$zone, 0)
df2$zone = as.character(df2$zone)

df3 = df2 |> 
  group_by(year, zone) |> 
  summarize(vpd_hour2 = mean(vpd_hour2))

df3$var = "vpd_hour"
colnames(df3)[3] = "value"

ggplot(df3, aes(x=year, y=value, col = zone))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  labs(x = NULL,
       y = "Mean of daily hours (VPD ≥ 0.75)",
       col = NULL)+
  theme_minimal()

df_all = rbind(df_all, df3)

#Analysis and graphics ---------------------------------------------------------
head(df_all)

#Climatic Zones
#--------------------------------------------------------------
# Class |	Critery (days) | Name                               |
#--------------------------------------------------------------
# 0	    | 0–49           | Low Persistence Zone (LP)          |
# 1 	  | 50–100         | Short Persistence Zone (SP)        |
# 2     | 101–149        | Intermediate Persistence Zone (IP) |
# 3     | ≥150           | Prolonged Persistence Zone (PP)    |
#--------------------------------------------------------------

df_all$zone[df_all$zone == "0"] <- "LP"
df_all$zone[df_all$zone == "1"] <- "SP"
df_all$zone[df_all$zone == "2"] <- "IP"
df_all$zone[df_all$zone == "3"] <- "PP"

df_all$zone = factor(df_all$zone, levels = c("LP", "SP", "IP", "PP"))

df_all$var[df_all$var == "vpd_mean"] <- "a) Annual mean VPD (kPa)"
df_all$var[df_all$var == "vpd_95"] <- "b) Annual 95th percentile VPD (kPa)"
df_all$var[df_all$var == "cons_vpd"] <- "c) Max. consecutive VPD ≥ 0.75 days"
df_all$var[df_all$var == "vpd_hour"] <- "d) Mean duration of VPD ≥ 0.75 (hours/day)"

#Graphic -----------------------------------------------------------------------
gg1 = ggplot(df_all, aes(x=year, y=value, col = zone))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = NULL, y = NULL, col = NULL)+
  #facet_wrap(~factor(var), scales = "free_y")+
  facet_wrap(~factor(var, c("a) Annual mean VPD (kPa)",
                            "b) Annual 95th percentile VPD (kPa)",
                            "c) Max. consecutive VPD ≥ 0.75 days",
                            "d) Mean duration of VPD ≥ 0.75 (hours/day)")), scales = "free")+
  scale_color_manual(values = c("#ffe101", "#fdbf6f", "#ff7700", "#dc1010"))+
  theme_bw()+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold")); gg1

ggsave(gg1,
       filename = "G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Paper1_VPD_ERA5/H1/Time_Series_H1_(since1975)_B.png",
       dpi = 600, units = "cm", height = 14, width = 16)












