#Proccess and Analysis extracting (FIRE) - H2
#Eduardo Q Marques 24-11-2026

library(terra)
library(sf)
library(tidyverse)

#Load data ---------------------------------------------------------------------
#Driest Period length
vpd_zone = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/Amazon_cons_VPD_Zone.tif")
names(vpd_zone) = "zone"
plot(vpd_zone)

#Mapbiomas
scf = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2024B.tif")
plot(scf)

fire_freq = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/Fire/MB_Fire_frequency_1985_2025.tiff")
plot(fire_freq)

fire_last = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/Fire/MB_Fire_Last_year_2025.tif")
plot(fire_last)

#Extrating values --------------------------------------------------------------
#total <- ncell(scf)

#bloco <- 1e6  #Processing by 1 million blocks

#Way to output
#caminho_saida <- "G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Paper1_VPD_ERA5/H2/Dados_pixel_analises_FIRE_H2.csv"

#start.time <- Sys.time()
#first_block <- TRUE

#for (ini in seq(1, total, by = bloco)) {
#  fim <- min(ini + bloco - 1, total)
#  idx_bloco <- ini:fim

#  cat(sprintf("Pixels %.0f of %.0f to get %.0f (%.1f%%)\n",
#              ini, fim, total, 100 * fim / total))

#  val_bloco <- scf[idx_bloco][, 1]
#  validos_bloco <- !is.na(val_bloco) #To not proccess NAs

#  if (any(validos_bloco)) {
#    idx_validos <- idx_bloco[validos_bloco]
#    val_validos <- val_bloco[validos_bloco]
#    xy_bloco <- xyFromCell(scf, idx_validos)

#    df_zone <- terra::extract(vpd_zone, xy_bloco) #Extract values from zone
#    df_freq <- terra::extract(fire_freq, xy_bloco) #Extract values from zone
#    df_year <- terra::extract(fire_last, xy_bloco) #Extract values from zone
#    df_bloco = cbind(df_zone, df_freq, df_year)

#Include xy
#    df_bloco$age <- as.numeric(val_validos)
#    df_bloco$x   <- xy_bloco[, 1]
#    df_bloco$y   <- xy_bloco[, 2]

#Saving CSV (create first time and append after)
#    write.table(df_bloco, caminho_saida,
#                sep = ",", dec = ".",
#                row.names = FALSE,
#                col.names = first_block,
#                append = !first_block)

#    first_block <- FALSE
#  }

#Clean memory
#  rm(val_bloco, validos_bloco, xy_bloco, df_bloco)
#  if (exists("idx_validos")) rm(idx_validos, val_validos)
#  gc()
#}

#print(Sys.time() - start.time) #Proccessing in 1.87hr (Laptop)

#Analysis and graphics ---------------------------------------------------------
df = read_csv("home/leaf/Documentos/Paper_VPD_Marques_et_al/Dados_pixel_analises_FIRE_H2.csv") #workstation
#df = read_csv("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Paper1_VPD_ERA5/H2/Dados_pixel_analises_FIRE_H2.csv") #leptop
colnames(df)[2:3] = c("fire_freq", "fire_last")

#Climatic Zones
#--------------------------------------------------------------
# Class |	Critery (days) | Name                               |
#--------------------------------------------------------------
# 0	    | 0–49           | Low Persistence Zone (LP)          |
# 1 	  | 50–100         | Short Persistence Zone (SP)        |
# 2     | 101–149        | Intermediate Persistence Zone (IP) |
# 3     | ≥150           | Prolonged Persistence Zone (PP)    |
#--------------------------------------------------------------
df$zone[df$zone == "0"] <- "LP"
df$zone[df$zone == "1"] <- "SP"
df$zone[df$zone == "2"] <- "IP"
df$zone[df$zone == "3"] <- "PP"

df$zone = factor(df$zone, levels = c("LP", "SP", "IP", "PP"))
head(df)

#Summarize data ----------------------------------------------------------------
#Fire frequency
df_freq = df |> 
  na.omit() |> 
  group_by(zone, age) |> 
  summarise(fire_freq = mean(fire_freq),
            n = n())

#Last year fire
df_last = df |> 
  na.omit() |> 
  group_by(zone, age) |> 
  summarise(fire_last = median(fire_last),
            n = n())

#Grafics -----------------------------------------------------------------------
#Fire frequency
gg_freq = ggplot(df, aes(x=zone, y=fire_freq, col = age))+
  geom_jitter(size = 2, alpha = 0.8)+
  stat_summary(fun = mean, geom = "point", col = "black", size = 2)+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", col = "black", width = 0.1)+
  # labs(x = "Age", y = "Fire Frequency (1985-2024)", col = "Zone")+
  #scale_color_manual(values = c("#ffe101", "#fdbf6f", "#ff7700", "#dc1010"))+
  theme_bw(); gg_freq

ggsave(gg_freq,
       filename = "G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Paper1_VPD_ERA5/H2/SF_Age_Fire_freq_jt_H2.png",
       dpi = 600, units = "cm", height = 11, width = 16)


gg_freq_lm = ggplot(df_freq, aes(x=age, y=fire_freq, col = zone))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  labs(x = "Age", y = "Fire Frequency (1985-2024)", col = "Zone")+
  scale_color_manual(values = c("#ffe101", "#fdbf6f", "#ff7700", "#dc1010"))+
  theme_bw()+
  theme(legend.position = "right"); gg_freq_lm


ggsave(gg_freq_lm,
       filename = "G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Paper1_VPD_ERA5/H2/SF_Age_Fire_freq_lm_H2.png",
       dpi = 600, units = "cm", height = 11, width = 16)



#Models ------------------------------------------------------------------------
summary(lm(age ~ fire_freq*zone, data = df))
summary(lm(age ~ fire_freq*zone, data = df_freq))




















