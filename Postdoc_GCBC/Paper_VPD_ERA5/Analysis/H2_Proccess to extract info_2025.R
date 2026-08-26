#Paper VPD ERA5 Amazon - Proccess to extract info - 2025
# H2 - Along the successional gradient, young secondary forests
#remain exposed for longer periods to microclimatic conditions
#that favor fire spread throughout the dry season.

#Eduardo Q Marques 26-08-2026

library(terra)
library(tidyverse)

#Load data ---------------------------------------------------------------------
#Secondary Forest
#Workstation
scf24 = rast("/home/leaf/Documentos/Paper_VPD_Marques_et_al/Rasters_H2/MB_Forest_age_2024B.tif")
plot(scf24)

scf25 = scf24 + 1
#scf25 = ifel(scf25 < 1, NA, scf25)
plot(scf25)

#VPD >= 075 kPa hours by month
#Workstation
list_rst = list.files("/home/leaf/Documentos/Paper_VPD_Marques_et_al/Rasters_H2/VPD_month_2025", full.names = T); list_rst

h_vpd = rast(list_rst)
plot(h_vpd)

#Extracting by random points -----------------------------------------------
smp <- spatSample(scf25, size = 1000000, method = "random",
                  as.points = TRUE, na.rm = TRUE)

plot(scf25)
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

write.csv(df3, "Hours_VPD75_Age_full_2025.csv", row.names = F)

df4 = df3 |> 
  na.omit() |> 
  group_by(Age, Month) |> 
  summarise(Hours = mean(Hours),
            n = n())

write.csv(df4, "Hours_VPD75_Age_2025.csv", row.names = F)


ggplot(df4, aes(x=Age, y=Hours))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(df4, aes(x=Age, y=Hours, col=Month))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month, scale = "free")


