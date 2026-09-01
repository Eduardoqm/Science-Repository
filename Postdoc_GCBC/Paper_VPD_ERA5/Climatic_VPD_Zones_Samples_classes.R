#Climatic Zones samples by classes

#Eduardo Q Marques 31-08-2026

library(terra)

#Load data ---------------------------------------------------------------------
setwd("/home/leaf/Documentos/Paper_VPD_Marques_et_al/Rasters_H2")
dir()

zone = vect("Amazon_VPD_Zone.shp")
plot(zone)

mb = rast("MapBiomas_2024_col10.tiff")
sf = rast("MB_Forest_age_2024B.tif")
fire = rast("MB_Fire_frequency_1985_2025.tiff")

#Reclass Forest burned and not burned ------------------------------------------
#Land Use and Cover
mb2 = ifel(mb %in% c(1,3,6,4,7,5,49), 1, NA) #Binary
mb3 = ifel(is.na(sf), mb2, NA) #Filter Scondary

#Secondary Forest
sf2 = ifel(sf < 39, NA, 1) #Binary

#Fire
fire2 = ifel(fire < 1, 0, 1) #Binary

#Classifying
mb_fire = ifel(fire > 1, mb3, NA)
mb_intac = ifel(fire < 1, mb3, NA)
sf_fire = ifel(fire > 1, sf2, NA)
sf_intac = ifel(fire < 1, sf2, NA)

#Masking and Samplying ---------------------------------------------------------



















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

