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
mb_fire = ifel(fire > 1, mb3, NA) #Primary Forest burned
mb_intac = ifel(fire < 1, mb3, NA) #Primary Forest intact
sf_fire = ifel(fire > 1, sf2, NA) #Secondary Forest burned
sf_intac = ifel(fire < 1, sf2, NA) #Secondary Forest intact

#writeRaster(mb_fire, "MB_PriForest_burned.tiff")
#writeRaster(mb_intac, "MB_PriForest_intact.tiff")
#writeRaster(sf_fire, "MB_SecForest_burned.tiff")
#writeRaster(sf_intac, "MB_SecForest_intact.tiff")

#Masking -----------------------------------------------------------------------
# 1) Non-Seasonal Zone (NS)
# 2) Short Seasonal Zone (SZ)
# 3) Intermediate Seasonal Zone (IS)
# 4) Prolonged Seasonal Zone (PS)

#Primary Forest burned
mb_fire_ns = mask(crop(mb_fire, zone[1]), zone[1])
mb_fire_sz = mask(crop(mb_fire, zone[2]), zone[2])
mb_fire_is = mask(crop(mb_fire, zone[3]), zone[3])
mb_fire_ps = mask(crop(mb_fire, zone[4]), zone[4])

#Primary Forest intact
mb_intac_ns = mask(crop(mb_intac, zone[1]), zone[1])
mb_intac_sz = mask(crop(mb_intac, zone[2]), zone[2])
mb_intac_is = mask(crop(mb_intac, zone[3]), zone[3])
mb_intac_ps = mask(crop(mb_intac, zone[4]), zone[4])

#Secondary Forest burned
sf_fire_ns = mask(crop(sf_fire, zone[1]), zone[1])
sf_fire_sz = mask(crop(sf_fire, zone[2]), zone[2])
sf_fire_is = mask(crop(sf_fire, zone[3]), zone[3])
sf_fire_ps = mask(crop(sf_fire, zone[4]), zone[4])

#Secondary Forest intact
sf_intac_ns = mask(crop(sf_intac, zone[1]), zone[1])
sf_intac_sz = mask(crop(sf_intac, zone[2]), zone[2])
sf_intac_is = mask(crop(sf_intac, zone[3]), zone[3])
sf_intac_ps = mask(crop(sf_intac, zone[4]), zone[4])

#Sampling ----------------------------------------------------------------------












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

