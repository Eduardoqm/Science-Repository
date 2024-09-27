#Amazonian Validations Analysis - Olofsson Accuracy

#Eduardo Q Marques 05-07-2024

library(tidyverse)
library(terra)
library(mapaccuracy)

#Load Classes Data -------------------------------------------------------------
setwd("G:/My Drive/Postdoc_UFRA/Papers/Culturas_permanentes (Silverio et al)/Amazonia_validation")
dir()

df = read.csv("Validation_Classes_Result.csv")

#Remove transitions from forest to others classes that make no sense -----------
df2 = df

#Excluding Forest Transition
df2$MB_id[df2$MB_id == 3 & df2$MB_Master_Class != 3] <- NA

#Merge Agriculture
df2$MB_Master_Class[df2$MB_Master_Class %in% c(19,39,20,40,62,41)] = 18

#Merge Perennial Crop
df2$MB_Master_Class[df2$MB_Master_Class %in% c(46,47,35,48)] = 36

df3 = df2 %>%
  #filter(MB_Master_Class %in% c(unique(MB_id))) %>% #Selecting same classes of Mapbiomas
  filter(MB_Master_Class != 0) %>% #Remove years that dont match
  na.omit()

#MapBiomas 2022 ----------------------------------------------------------------
#mb22 = rast("G:/My Drive/Postdoc_UFRA/Geodata/Rasters/MapBiomes_Brazil/brasil_coverage_2022.tif")
#bio = vect("G:/My Drive/Postdoc_UFRA/Geodata/Vectors/biomas.shp")
#mb22b = terra::crop(mb22, bio[5], ext = T)
#am22 = terra::mask(mb22b, bio[5])
#setwd("G:/My Drive/Postdoc_UFRA/Geodata/Rasters/MapBiomes_Brazil")
#writeRaster(am22, "amazonia_2022.tif", overwrite=TRUE)
#plot(am22)

am22 = rast("G:/My Drive/Postdoc_UFRA/Geodata/Rasters/MapBiomes_Brazil/amazonia_2022.tif")
plot(am22)

am22b = am22
am22b[am22b %in% c(19,39,20,40,62,41)] = 18 #Merge Agriculture
am22b[am22b %in% c(46,47,35,48)] = 36 #Merge Perennial Crop
plot(am22b)

nclass = freq(am22b)
nclass = nclass[-1,-1]
colnames(nclass)[1] = c("class")
nclass$area_ha = (nclass$count*900)/10000
#nclass2 = nclass %>% filter(class %in% c(unique(df3$MB_id)))

#Olofsson Accuracy -------------------------------------------------------------
r = df3$MB_id
m = df3$MB_Master_Class
Nh = nclass$area_ha
names(Nh) = nclass$class

olf = olofsson(r, m, Nh)
olf

#Making and saving tables ------------------------------------------------------
UA = as.data.frame(olf[["UA"]])
PA = as.data.frame(olf[["PA"]])
area = as.data.frame(olf[["area"]])
SEua = as.data.frame(olf[["SEua"]])
SEpa = as.data.frame(olf[["SEpa"]])
SEa = as.data.frame(olf[["SEa"]])

olf2 = cbind(UA, PA, area, SEua, SEpa, SEa)
olf2$class = c(3,4,5,6,9,11,12,15,18,21,23,24,25,29,30,32,33,36,100,27,13)

setwd("G:/My Drive/Postdoc_UFRA/Papers/Culturas_permanentes (Silverio et al)/Amazonia_validation/Accuracy_Test")

write.csv(olf2, file = "Results_Olofsson_B.csv", row.names = F)

mtx = as.data.frame(olf[["matrix"]])
write.csv(mtx, file = "Matrix_Olofsson_B.csv", row.names = T)

