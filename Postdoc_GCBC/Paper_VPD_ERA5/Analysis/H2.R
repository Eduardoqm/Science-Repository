#Paper VPD ERA5 Amazon
# H2 - Along the successional gradient, young secondary forests
#remain exposed for longer periods to microclimatic conditions
#that favor fire spread throughout the dry season.

#Eduardo Q Marques 20-08-2026

library(terra)
library(tidyverse)

# 1. Verificar quantas threads/núcleos o terra reconhece
getOption("terraNumThreads")

# 2. Forçar o terra a usar todos os 28 núcleos disponíveis
terraOptions(threads = 28)

# 3. Se você estiver usando funções que aceitam paralelismo interno (como app, predict, etc.)
# ou operações de I/O do raster, garanta que o multithread esteja ativo:
terraOptions(parallel = TRUE)

#Load data ---------------------------------------------------------------------
#Secondary Forest
#Leptop
#scf23 = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2023.tif")
#scf24 = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2023.tif")

#Workstation
#scf24 = rast("/home/leaf/Documentos/Paper_VPD_Marques_et_al/Rasters_H2/MB_Forest_age_2023.tif")
scf24 = rast("/home/leaf/Documentos/Paper_VPD_Marques_et_al/Rasters_H2/MB_Forest_age_2024.tif")

#plot(scf23)
plot(scf24)

#VPD >= 075 kPa hours by month
#Leptop
#list_rst = list.files("G:/My Drive/GEE_VPD_Horas_2024", full.names = T); list_rst

#Workstation
list_rst = list.files("/home/leaf/Documentos/Paper_VPD_Marques_et_al/Rasters_H2/VPD_month", full.names = T); list_rst

h_vpd = rast(list_rst)
plot(h_vpd)

#Extracting by stratified points -----------------------------------------------
freq(scf24)
scf24B = round(scf24)
freq(scf24B)
plot(scf24B)

smp <- spatSample(scf24B, size = 5, method = "stratified", 
                         as.points = TRUE, na.rm = TRUE)

plot(scf24B)
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

df3$Age = round(df3$Age, digits = 0)

write.csv(df3, "Hours_VPD75_Age_full.csv", row.names = F)

df4 = df3 |> 
  na.omit() |> 
  group_by(Age, Month) |> 
  summarise(Hours = mean(Hours),
            n = n())

ggplot(df4, aes(x=Age, y=Hours))+
  geom_point()+
  geom_smooth(method = "lm")


ggplot(df4, aes(x=Age, y=Hours, col=Month))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month, scale = "free")














#Get only SF pixels ------------------------------------------------------------
#h_vpd2 = resample(h_vpd, scf24, method = "average")
scf24b = resample(scf24, h_vpd[[1]], method = "average")
plot(scf24b)

#Percentage of SF
scf_count = ifel(scf24 < 1, 0, 1)
scf_count = resample(scf_count, h_vpd[[1]], method = "sum")
plot(scf_count)

scf_perc = (scf_count/80000)*100
plot(scf_perc)

#age = ifel(scf_perc <50, NA, scf24b)


#Stack Age and Percentage of SF ------------------------------------------------
h_vpd = c(h_vpd, scf24b, scf_perc)

df_vpd = as.data.frame(h_vpd)

colnames(df_vpd) = c("Jan", "Fev", "Mar", "April", "May", "June",
                     "July", "Aug", "Set", "Oct", "Nov", "Dec",
                     "Age", "Perc_SF")


df_vpd2 = df_vpd |> 
  na.omit() |> 
  filter(Perc_SF >= 50) |> 
  pivot_longer(
    cols = c(Jan, Fev, Mar, April, May, June, July, Aug, Set, Oct, Nov, Dec), 
    names_to = "Month", 
    values_to = "Hours")

ggplot(df_vpd2, aes(x=Age, y=Hours))+
  geom_point()+
  geom_smooth()


ggplot(df_vpd2, aes(x=Age, y=Hours, col=Month))+
  #geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month, scale = "free")




