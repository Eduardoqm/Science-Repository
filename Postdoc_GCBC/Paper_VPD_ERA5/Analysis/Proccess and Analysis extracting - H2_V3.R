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


#Filter repeated values from ERA5
#library(DescTools) #To use Mode()


df2 = df |> 
  group_by(mean, mean_vpd_dp, q95_vpd_dp, max_consecutive_vpd_days, annual_vpd_hours_DP) |> 
  summarise(age_mean = mean(age),
            n = n()) |> 
  na.omit() |> 
  filter(n >= 20000)

colnames(df2) = c("zone", "vpd_mean", "vpd_95", "cons_vpd", "vpd_hour", "age", "n")

#df2 = df |> 
#  group_by(age, mean) |> 
#  summarise(vpd_mean = mean(mean_vpd_dp),
#            vpd_95 = mean(q95_vpd_dp),
#            cons_vpd = mean(max_consecutive_vpd_days),
#            vpd_hour = mean(annual_vpd_hours_DP)) |> 
#  na.omit()
#colnames(df2)[2] = "zone"

head(df2)



df3 <- df2 |> 
  pivot_longer(
    cols = c(vpd_mean, vpd_95, cons_vpd, vpd_hour),
    names_to = "var",
    values_to = "value")

#Climatic Zones
#--------------------------------------------------------------
# Class |	Critery (days) | Name                               |
#--------------------------------------------------------------
# 0	    | 0–49           | Low Persistence Zone (LP)          |
# 1 	  | 50–100         | Short Persistence Zone (SP)        |
# 2     | 101–149        | Intermediate Persistence Zone (IP) |
# 3     | ≥150           | Prolonged Persistence Zone (PP)    |
#--------------------------------------------------------------

df3$zone[df3$zone == "0"] <- "LP"
df3$zone[df3$zone == "1"] <- "SP"
df3$zone[df3$zone == "2"] <- "IP"
df3$zone[df3$zone == "3"] <- "PP"

df3$zone = factor(df3$zone, levels = c("LP", "SP", "IP", "PP"))

df3$var[df3$var == "vpd_mean"] <- "a) Annual mean VPD (kPa)"
df3$var[df3$var == "vpd_95"] <- "b) Annual 95th percentile VPD (kPa)"
df3$var[df3$var == "cons_vpd"] <- "c) Max. consecutive VPD ≥ 0.75 days"
df3$var[df3$var == "vpd_hour"] <- "d) Mean duration of VPD ≥ 0.75 (hours/day)"


df3$age = round(df3$age, 0)

df4 = df3 |> 
  na.omit() |> 
  group_by(zone, age, var) |> 
  summarise(value = mean(value),
            n = n())


#Graphic -----------------------------------------------------------------------

limiar_line = data.frame(var = c("a) Annual mean VPD (kPa)"),
                         yint = c(0.75, 0.75))

gg1 = ggplot(df4, aes(x=age, y=value, col = zone))+
  geom_point()+
  geom_smooth(se = F)+
  #geom_line(aes(x = year, y = pred), color = "darkgray", linewidth = 1, inherit.aes = FALSE)+
  labs(x = NULL, y = NULL, col = NULL)+
  geom_hline(data = limiar_line, aes(yintercept = yint), linetype = "dashed")+
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
       filename = "G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Paper1_VPD_ERA5/H2/Secondary_Age_H2.png",
       dpi = 600, units = "cm", height = 14, width = 16)
















































