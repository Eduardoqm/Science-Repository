#Amazonian Validations Process

#Extract MapBiomas Classes by Validation Points

#Eduardo Q Marques 04-07-2024

library(terra)

#Load Data ---------------------------------------------------------------------
#Points
setwd("G:/My Drive/Postdoc_UFRA/Papers/Culturas_permanentes (Silverio et al)/Amazonia_validation")
dir()
am = vect("Amazonia_full_V2.shp")

#MapBiome Rasters
setwd("G:/My Drive/Postdoc_UFRA/Geodata/Rasters/MapBiomes_Brazil")
dir()

mb = list.files()
mb = mb[-1];mb
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

df = df[,-1]

am2 = cbind(am, df)

#Filter Years ------------------------------------------------------------------
#Transforming different year in zero
am2$brasil_coverage_1985[am2$year != 1985] = 0
am2$brasil_coverage_2000[am2$year != 2000] = 0
am2$brasil_coverage_2001[am2$year != 2001] = 0
am2$brasil_coverage_2002[am2$year != 2002] = 0
am2$brasil_coverage_2003[am2$year != 2003] = 0
am2$brasil_coverage_2004[am2$year != 2004] = 0
am2$brasil_coverage_2005[am2$year != 2005] = 0
am2$brasil_coverage_2006[am2$year != 2006] = 0
am2$brasil_coverage_2007[am2$year != 2007] = 0
am2$brasil_coverage_2008[am2$year != 2008] = 0
am2$brasil_coverage_2009[am2$year != 2009] = 0
am2$brasil_coverage_2010[am2$year != 2010] = 0
am2$brasil_coverage_2011[am2$year != 2011] = 0
am2$brasil_coverage_2012[am2$year != 2012] = 0
am2$brasil_coverage_2013[am2$year != 2013] = 0
am2$brasil_coverage_2014[am2$year != 2014] = 0
am2$brasil_coverage_2015[am2$year != 2015] = 0
am2$brasil_coverage_2016[am2$year != 2016] = 0
am2$brasil_coverage_2017[am2$year != 2017] = 0
am2$brasil_coverage_2018[am2$year != 2018] = 0
am2$brasil_coverage_2019[am2$year != 2019] = 0
am2$brasil_coverage_2020[am2$year != 2020] = 0
am2$brasil_coverage_2021[am2$year != 2021] = 0
am2$brasil_coverage_2022[am2$year %in% c(1985:2021)] = 0

#Sum classes
am2df = as.data.frame(am2[,c(8:31)])

am2df$MB_Master_Class = 0

for (z in 1:length(am2df$brasil_coverage_1985)) {
  am2df[z,25] = max(am2df[z,])
}

am2$MB_Master_Class = am2df$MB_Master_Class
View(as.data.frame(am2))

#Saving Results ----------------------------------------------------------------
setwd("G:/My Drive/Postdoc_UFRA/Papers/Culturas_permanentes (Silverio et al)/Amazonia_validation")

am2df2 = as.data.frame(am2)
write.csv(am2df2, "Validation_Classes_Result.csv", row.names = F)

am_sf = sf::st_as_sf(am2)
sf::st_write(am_sf, "Amazonia_full_V4.shp")




