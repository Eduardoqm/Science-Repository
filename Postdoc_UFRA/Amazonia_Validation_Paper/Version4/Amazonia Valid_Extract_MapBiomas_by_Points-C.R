#Amazonian Validations Process

#Extract MapBiomas Classes by Validation Points

#Eduardo Q Marques 04-07-2024 Updated: 13-09-2024

library(terra)

#Load Data ---------------------------------------------------------------------
#Points
setwd("G:/My Drive/Postdoc_UFRA/Papers/Amazonia_validation (Marques et al)/Shapes")
dir()
am = vect("Amazonia_full_V2.shp")

#MapBiome Rasters
setwd("G:/My Drive/Postdoc_UFRA/Geodata/Rasters/MapBiomes_Brazil/Collection_9")
dir()

mb = list.files()
#plot(rast(mb[1]))
#plot(am, add = T)

#Extract MapBiomes information -------------------------------------------------
df = terra::extract(rast(mb[1]), am)

for (z in 2:length(mb)) {
  print(mb[z])
  dfx = terra::extract(rast(mb[z]), am)
  df = cbind(df, dfx)
  df = df[,-(z+1)]
}

head(df)

df = df[,-1]

am2 = cbind(am, df)
View(data.frame(am2))

#Filter Years ------------------------------------------------------------------
#Transforming different year in zero
am2$classification_2000[am2$year != 2000] = 0
am2$classification_2001[am2$year != 2001] = 0
am2$classification_2002[am2$year != 2002] = 0
am2$classification_2003[am2$year != 2003] = 0
am2$classification_2004[am2$year != 2004] = 0
am2$classification_2005[am2$year != 2005] = 0
am2$classification_2006[am2$year != 2006] = 0
am2$classification_2007[am2$year != 2007] = 0
am2$classification_2008[am2$year != 2008] = 0
am2$classification_2009[am2$year != 2009] = 0
am2$classification_2010[am2$year != 2010] = 0
am2$classification_2011[am2$year != 2011] = 0
am2$classification_2012[am2$year != 2012] = 0
am2$classification_2013[am2$year != 2013] = 0
am2$classification_2014[am2$year != 2014] = 0
am2$classification_2015[am2$year != 2015] = 0
am2$classification_2016[am2$year != 2016] = 0
am2$classification_2017[am2$year != 2017] = 0
am2$classification_2018[am2$year != 2018] = 0
am2$classification_2019[am2$year != 2019] = 0
am2$classification_2020[am2$year != 2020] = 0
am2$classification_2021[am2$year != 2021] = 0
am2$classification_2022[am2$year != 2022] = 0
am2$classification_2023[am2$year %in% c(1985:2022)] = 0
#am2$classification_2023[am2$year < 2000] = 0

View(data.frame(am2))

#Sum classes
am2df = as.data.frame(am2[,c(6:29)])

am2df$MB_Master_Class = 0

for (z in 1:length(am2df$classification_2000)) {
  am2df[z,25] = max(am2df[z,])
}

#Adjust MapBiomas Level 4 to Level 3, 2 and 1 ----------------------------------
am2df$MB_lv4 = am2df$MB_Master_Class
am2df$MB_lv3 = am2df$MB_Master_Class

#Level 3
am2df$MB_lv3[am2df$MB_lv3 %in% c(39,20,40,62,41)] = 19 #Temporary Crop
am2df$MB_lv3[am2df$MB_lv3 %in% c(46,47,35,48)] = 36 #Perennial Crop

#Level 2
am2df$MB_lv2 = am2df$MB_lv3
am2df$MB_lv2[am2df$MB_lv2 %in% c(19,36)] = 18 #Agriculture

#Level 1
am2df$MB_lv1 = am2df$MB_lv2

am2df$MB_lv1[am2df$MB_lv1 %in% c(3,4,5,6,49)] = 1 #Forest
am2df$MB_lv1[am2df$MB_lv1 %in% c(11,12,32,29,50,13)] = 10 #Non Forest Natural Formation
am2df$MB_lv1[am2df$MB_lv1 %in% c(9,15,18,21)] = 14 #Farming
am2df$MB_lv1[am2df$MB_lv1 %in% c(23,24,25,30)] = 22 #Non vegetated area 
am2df$MB_lv1[am2df$MB_lv1 %in% c(33,31)] = 26 #Water 


#Imputing results in the shape -------------------------------------------------
am2$MB_lv4 = am2df$MB_lv4
am2$MB_lv3 = am2df$MB_lv3
am2$MB_lv2 = am2df$MB_lv2
am2$MB_lv1 = am2df$MB_lv1

am2 = am2[,c(1,3,4,30,31,32,33)]

View(data.frame(am2))

#Adjust our validation to Level 3, 2 and 1 -------------------------------------
am2$Val_lv3 = am2$MB_id

#Level 3
am2$Val_lv3[am2$Val_lv3 %in% c(39,20,40,62,41)] = 19 #Temporary Crop
am2$Val_lv3[am2$Val_lv3 %in% c(46,47,35,48)] = 36 #Perennial Crop

#Level 2
am2$Val_lv2 = am2$Val_lv3
am2$Val_lv2[am2$Val_lv2 %in% c(19,36)] = 18 #Agriculture

#Level 1
am2$Val_lv1 = am2$Val_lv2

am2$Val_lv1[am2$Val_lv1 %in% c(3,4,5,6,49)] = 1 #Forest
am2$Val_lv1[am2$Val_lv1 %in% c(11,12,32,29,50,13)] = 10 #Non Forest Natural Formation
am2$Val_lv1[am2$Val_lv1 %in% c(9,15,18,21)] = 14 #Farming
am2$Val_lv1[am2$Val_lv1 %in% c(23,24,25,30)] = 22 #Non vegetated area 
am2$Val_lv1[am2$Val_lv1 %in% c(33,31)] = 26 #Water 

am2 = am2[,c(1,2,8,9,10,4,5,6,7)]

View(data.frame(am2))


#Saving Results ----------------------------------------------------------------
setwd("G:/My Drive/Postdoc_UFRA/Papers/Amazonia_validation (Marques et al)/Shapes")

am2df2 = as.data.frame(am2)
write.csv(am2df2, "Validation_Classes_Result.csv", row.names = F)

am_sf = sf::st_as_sf(am2)
sf::st_write(am_sf, "Amazonia_full_V3.shp")




