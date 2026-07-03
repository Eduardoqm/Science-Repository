#Clustering Climatic Znes of Amazon by VPD

#Eduardo Q Marques 03-07-2026

library(terra)

#Load data ---------------------------------------------------------------------
setwd("G:/My Drive/GEE_VPD_Mensal")
dir()

list_rst = list.files()

month_vpd = rast(list_rst)
plot(month_vpd)

ray_vpd = ifel(month_vpd < 0.75, 0, 1)
plot(ray_vpd)

ray_vpd2 = sum(ray_vpd)
plot(ray_vpd2)

vpd_zone = ray_vpd2

vpd_zone[vpd_zone < 1] <- 0
vpd_zone[vpd_zone >= 1 & vpd_zone < 2] <- 1
vpd_zone[vpd_zone >= 2 & vpd_zone < 3] <- 2
vpd_zone[vpd_zone >= 3] <- 3

plot(vpd_zone)
