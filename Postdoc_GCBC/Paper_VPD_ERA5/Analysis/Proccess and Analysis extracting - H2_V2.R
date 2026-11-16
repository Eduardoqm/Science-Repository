#Proccess and Analysis extracting - H2
#Eduardo Q Marques 16-11-2026

library(terra)
library(tidyverse)

#Load data ---------------------------------------------------------------------
#VPD rasters (2024)
cons_vpd = rast("G:/My Drive/GEE_Max_Consecutive_VPD_Days_DP_Annual/max_consecutive_vpd_days_DP_2024.tif") #Consecutive VPD days > 0.75 kPa
vpd_95 = rast("G:/My Drive/GEE_Q95_VPD_DP_Annual/q95_vpd_dp_2024.tif") #Annual quantile 95 VDP
vpd_mean = rast("G:/My Drive/GEE_Mean_VPD_DP_Annual/mean_vpd_dp_2024.tif") #Annual mean VDP
vpd_hour = rast("G:/My Drive/GEE_Annual_VPD_Hours_DP/annual_vpd_hours_DP_2024.tif") #Hours VPD > 0.75 kPa

plot(cons_vpd)
plot(vpd_95)
plot(vpd_mean)
plot(vpd_hour)

#Driest Period length
vpd_zone = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/Amazon_cons_VPD_Zone.tif")
vpd_zone
plot(vpd_zone)

#Mapbiomas
scf = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2024B.tif")
plot(scf)

#Amazonia limits
am = vect("G:/My Drive/Geodata/Vectors/Amazonia.shp")
plot(am, add = T)

#Clipping to Amazonia Biome ----------------------------------------------------
cons_vpd = mask(crop(cons_vpd, am), am)
vpd_95 = mask(crop(vpd_95, am), am)
vpd_mean = mask(crop(vpd_mean, am), am)
vpd_hour = mask(crop(vpd_hour, am), am)
vpd_zone = mask(crop(vpd_zone, am), am)
#scf = mask(crop(scf, am), am)

#Resampling rasters ------------------------------------------------------------
vpd_zone = resample(vpd_zone, cons_vpd, method = "average"); plot(vpd_zone)
scf2 = resample(scf, cons_vpd, method = "average"); plot(scf2)

scf_count = ifel(scf > 0, 1, 0); plot(scf_count)
scf_count = resample(scf_count, cons_vpd, method = "sum"); plot(scf_count)


#setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters")
#writeRaster(mb3, "MB_sum_pixel_Forest_2025_resampled.tiff", overwrite = T)


#Consecutive days in flammability conditions VPD > 0.75 ------------------------
scf_age = as.data.frame(scf2, na.rm = F); head(scf_age)
scf_count2 = as.data.frame(scf_count, na.rm = F); head(scf_count2)
vpd_zone_df = as.data.frame(vpd_zone, na.rm= F); head(vpd_zone_df)

df = as.data.frame(cons_vpd, na.rm = F)
colnames(df) = "cons_vpd"
df$zone = vpd_zone_df$mean
df$age = scf_age$MB_SF_Age_2024
df$scf_p = scf_count2$MB_SF_Age_2024

df2 = df |>
  filter(scf_p > 0) |> 
  na.omit()

df2$age = as.numeric(df2$age)
df2$zone = round(df2$zone, 0)
df2$zone = as.character(df2$zone)

df3 = df2 |> 
  group_by(age, zone) |> 
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

#write.csv(df_all, "G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Paper1_VPD_ERA5/H1/Dados_analises_H1.csv", row.names = F)



















































