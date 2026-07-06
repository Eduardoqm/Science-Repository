#Clustering Climatic Zones of Amazon by VPD

#Eduardo Q Marques 03-07-2026

library(terra)

#Load data ---------------------------------------------------------------------
setwd("G:/My Drive/GEE_VPD_Mensal")
dir()

list_rst = list.files()
month_vpd = rast(list_rst)
plot(month_vpd)

#Binary segmentation to limier of inflammable VPD (0.75 kPa) -------------------
ray_vpd = ifel(month_vpd < 0.75, 0, 1)
plot(ray_vpd)

ray_vpd2 = sum(ray_vpd) #Sum of months with pixels
plot(ray_vpd2)

#Clustarization by total months within VPD limier ------------------------------
vpd_zone = ray_vpd2

#Classifying
vpd_zone[vpd_zone < 1] <- 0
vpd_zone[vpd_zone >= 1 & vpd_zone <= 3] <- 1
vpd_zone[vpd_zone > 3 & vpd_zone < 6] <- 2
vpd_zone[vpd_zone >= 6] <- 3

plot(vpd_zone)

#Convert raster to polygon -----------------------------------------------------
vpd_zone2 = as.polygons(vpd_zone)
plot(vpd_zone2)

writeVector(vpd_zone2, "Amazon_VPD_Zone.shp")

#Basic proportions -------------------------------------------------------------
df_zone = freq(vpd_zone)
df_zone$class = c("Zero months", "Until 3 months",
                  "3 to 6 months", "> 6 months")

df_zone$Area_km2 = df_zone$count*81
df_zone$Area_perc = (df_zone$count/sum(df_zone$count))*100

library(ggplot2)

ggplot(df_zone, aes(x=class, y=Area_perc))+
  geom_col()

