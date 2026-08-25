#Paper VPD ERA5 Amazon - Proccess to extract info
# H2 - Along the successional gradient, young secondary forests
#remain exposed for longer periods to microclimatic conditions
#that favor fire spread throughout the dry season.

#Eduardo Q Marques 20-08-2026

library(terra)
library(tidyverse)
library(parallel)

#Load data ---------------------------------------------------------------------
#Secondary Forest
#Leptop
#scf23 = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2023.tif")
#scf24 = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2023.tif")

#Workstation
#scf23 = rast("/home/leaf/Documentos/Paper_VPD_Marques_et_al/Rasters_H2/MB_Forest_age_2023.tif")
scf24 = rast("/home/leaf/Documentos/Paper_VPD_Marques_et_al/Rasters_H2/MB_Forest_age_2024B.tif")

#plot(scf23)
plot(scf24)

#VPD >= 075 kPa hours by month
#Leptop
#list_rst = list.files("G:/My Drive/GEE_VPD_Horas_2024", full.names = T); list_rst

#Workstation
list_rst = list.files("/home/leaf/Documentos/Paper_VPD_Marques_et_al/Rasters_H2/VPD_month", full.names = T); list_rst

h_vpd = rast(list_rst)
plot(h_vpd)

#Extracting by random points -----------------------------------------------
smp <- spatSample(scf24, size = 1000000, method = "random",
                  as.points = TRUE, na.rm = TRUE)

plot(scf24)
plot(smp, add = T)

df = as.data.frame(smp)

vpd = terra::extract(h_vpd, smp)

df2 = cbind(vpd, df)

colnames(df2) = c("id", "Jan", "Fev", "Mar", "April", "May", "June",
                     "July", "Aug", "Set", "Oct", "Nov", "Dec", "Age")

df3 = df2 |> 
  pivot_longer(
    cols = c(Jan, Fev, Mar, April, May, June, July, Aug, Set, Oct, Nov, Dec), 
    names_to = "Month", 
    values_to = "Hours")

#df3$Age = round(df3$Age, digits = 0)

write.csv(df3, "Hours_VPD75_Age_full.csv", row.names = F)

df4 = df3 |> 
  na.omit() |> 
  group_by(Age, Month) |> 
  summarise(Hours = mean(Hours),
            n = n())

write.csv(df4, "Hours_VPD75_Age.csv", row.names = F)


ggplot(df4, aes(x=Age, y=Hours))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(df4, aes(x=Age, y=Hours, col=Month))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month, scale = "free")


