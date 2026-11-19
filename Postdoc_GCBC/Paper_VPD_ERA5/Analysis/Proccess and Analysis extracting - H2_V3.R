#Proccess and Analysis extracting - H2
#Eduardo Q Marques 18-11-2026

library(terra)
library(sf)
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

dp_month = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/DP_length_months.tif") 

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
dp_month2 = resample(dp_month, cons_vpd, method = "average"); plot(dp_month2)
#scf2 = resample(scf, cons_vpd, method = "average"); plot(scf2)

vpd_hour2 = (vpd_hour/dp_month2)/30 #Convert annual total hours in hours/day


#Extrating values --------------------------------------------------------------
#brick <- c(vpd_zone, vpd_mean, vpd_95, cons_vpd, vpd_hour2)

#total <- ncell(scf)

#Processing by blocks
#bloco <- 1e6  #1 million

#Way to output
#caminho_saida <- "G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Paper1_VPD_ERA5/H2/Dados_pixel_analises_H2.csv"

#start.time <- Sys.time()
#primeiro_bloco <- TRUE


#for (ini in seq(1, total, by = bloco)) {
  
#  fim <- min(ini + bloco - 1, total)
#  idx_bloco <- ini:fim
  
#  cat(sprintf("Processing pixels %.0f of %.0f de %.0f (%.1f%%)\n",
#              ini, fim, total, 100 * fim / total))
  
#  val_bloco <- scf[idx_bloco][, 1]
  
#  validos_bloco <- !is.na(val_bloco) #To not proccess NAs
  
#  if (any(validos_bloco)) {
#    idx_validos <- idx_bloco[validos_bloco]
#    val_validos <- val_bloco[validos_bloco]
#    xy_bloco <- xyFromCell(scf, idx_validos)
    
#    df_bloco <- terra::extract(brick, xy_bloco) #Extract values from Brick
    
    #Include xy
#    df_bloco$age <- as.numeric(val_validos)
#    df_bloco$x   <- xy_bloco[, 1]
#    df_bloco$y   <- xy_bloco[, 2]
    
    #Saving CSV (create forst time and append after)
#    write.table(df_bloco, caminho_saida,
#                sep = ",", dec = ".",
#                row.names = FALSE,
#                col.names = primeiro_bloco,
#                append = !primeiro_bloco)
    
#    primeiro_bloco <- FALSE
#  }
  
  #Clean memory
#  rm(val_bloco, validos_bloco, xy_bloco, df_bloco)
#  if (exists("idx_validos")) rm(idx_validos, val_validos)
#  gc()
#}

#print(Sys.time() - start.time) #Proccessing in 2.18hr (Laptop)





#Analysis and graphics ---------------------------------------------------------
df = read_csv("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Paper1_VPD_ERA5/H2/Dados_pixel_analises_H2.csv")
head(df)


df2 = df |> 
  na.omit() |> 
  group_by(x, y) |> 
  summarise(age_mean = mean(age),
            zone  = mean(mean),
            vpd_mean = mean(mean_vpd_dp),
            vpd_95 = mean(q95_vpd_dp),
            cons_vpd = mean(max_consecutive_vpd_days),
            vpd_hour = mean(annual_vpd_hours_DP))



df2 = df |> 
  group_by(mean, mean_vpd_dp, q95_vpd_dp, max_consecutive_vpd_days, annual_vpd_hours_DP) |> 
  summarise(age_mean = base::mean(age), .groups = "drop")



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



















































