#Amazonian Validations Analysis - Olofsson Accuracy

#Eduardo Q Marques 05-07-2024 Updated: 03-02-2026

library(tidyverse)
library(terra)
library(mapaccuracy)

#Load Classes Data -------------------------------------------------------------
setwd("G:/My Drive/Research/Papers/Amazonia_validation (Marques et al)/Shapes")
dir()

df = read.csv("Validation_Classes_Result_V4.csv")

#Remove transitions from forest to others classes that make no sense -----------
df2 = df

#Excluding Forest Transition
#df2$Val_lv3[df2$Val_lv3 == 3 & df2$MB_lv3 != 3] <- NA

df3 = df2 %>%
  filter(MB_lv3 != 0) %>% #Remove years that dont match
  filter(Name_MB != "indefinifo") %>% #Remove no classified points
  na.omit()

write.csv(df3, "Validation_Classes_Result_Final.csv")

#MapBiomas 2022 ----------------------------------------------------------------
am23 = rast("G:/My Drive/Research/Geodata/Rasters/MapBiomes_Brazil/Collection_9/mb2023.tif")
plot(am23)

#Level 3
am_l3 = am23
am_l3[am_l3 %in% c(39,20,40,62,41)] = 19 #Temporary Crop
am_l3[am_l3 %in% c(46,47,35,48)] = 36 #Perennial Crop
plot(am_l3)

#Level 2
am_l2 = am_l3
am_l2[am_l2 %in% c(19,36)] = 18 #Agriculture
plot(am_l2)

#Level 1
am_l1 = am_l2
am_l1[am_l1 %in% c(3,4,5,6,49)] = 1 #Forest
am_l1[am_l1 %in% c(11,12,32,29,50,13)] = 10 #Non Forest Natural Formation
am_l1[am_l1 %in% c(9,15,18,21)] = 14 #Farming
am_l1[am_l1 %in% c(23,24,25,30)] = 22 #Non vegetated area 
am_l1[am_l1 %in% c(33,31)] = 26 #Water 
plot(am_l1)

#Calculation area for levels 3, 2 and 1 ----------------------------------------
#Level3
nclass3 = freq(am_l3)
nclass3 = nclass3[-1,-1]
colnames(nclass3)[1] = c("class")
nclass3$area_ha = (nclass3$count*900)/10000

#Level2
nclass2 = freq(am_l2)
nclass2 = nclass2[-1,-1]
colnames(nclass2)[1] = c("class")
nclass2$area_ha = (nclass2$count*900)/10000

#Level1
nclass1 = freq(am_l1)
nclass1 = nclass1[-1,-1]
colnames(nclass1)[1] = c("class")
nclass1$area_ha = (nclass1$count*900)/10000

#Olofsson Accuracy -------------------------------------------------------------
#Level3
r = df3$Val_lv3
m = df3$MB_lv3
Nh = nclass3$area_ha
names(Nh) = nclass3$class

olf3 = olofsson(r, m, Nh); olf3

#Making and saving tables
setwd("G:/My Drive/Research/Papers/Amazonia_validation (Marques et al)/Accuracy_Test/V4")

UA = as.data.frame(olf3[["UA"]])
PA = as.data.frame(olf3[["PA"]])
area = as.data.frame(olf3[["area"]])
SEua = as.data.frame(olf3[["SEua"]])
SEpa = as.data.frame(olf3[["SEpa"]])
SEa = as.data.frame(olf3[["SEa"]])

olf3b = cbind(UA, PA, area, SEua, SEpa, SEa)
olf3b$class = c(3,4,5,6,9,11,12,15,19,21,23,24,25,29,30,31,32,33,36,100,13)

mtx3 = as.data.frame(olf3[["matrix"]])

#write.csv(olf3b, file = "Results_Olofsson_lvl3_V4.csv", row.names = F)
#write.csv(mtx3, file = "Matrix_Olofsson_lvl3_V4.csv", row.names = T)
#write.csv(nclass3, file = "MB_AM_class_area_lvl3_V4.csv", row.names = F)

#Level2
r = df3$Val_lv2
m = df3$MB_lv2
Nh = nclass2$area_ha
names(Nh) = nclass2$class

olf2 = olofsson(r, m, Nh); olf2

#Making and saving tables
UA = as.data.frame(olf2[["UA"]])
PA = as.data.frame(olf2[["PA"]])
area = as.data.frame(olf2[["area"]])
SEua = as.data.frame(olf2[["SEua"]])
SEpa = as.data.frame(olf2[["SEpa"]])
SEa = as.data.frame(olf2[["SEa"]])

olf2b = cbind(UA, PA, area, SEua, SEpa, SEa)
olf2b$class = c(3,4,5,6,9,11,12,15,18,21,23,24,25,29,30,31,32,33,100,13)

mtx2 = as.data.frame(olf2[["matrix"]])

#write.csv(olf2b, file = "Results_Olofsson_lvl2_V4.csv", row.names = F)
#write.csv(mtx2, file = "Matrix_Olofsson_lvl2_V4.csv", row.names = T)
#write.csv(nclass2, file = "MB_AM_class_area_lvl2_V4.csv", row.names = F)

#Level1
r = df3$Val_lv1
m = df3$MB_lv1
Nh = nclass1$area_ha
names(Nh) = nclass1$class

olf1 = olofsson(r, m, Nh); olf1

#Making and saving tables
UA = as.data.frame(olf1[["UA"]])
PA = as.data.frame(olf1[["PA"]])
area = as.data.frame(olf1[["area"]])
SEua = as.data.frame(olf1[["SEua"]])
SEpa = as.data.frame(olf1[["SEpa"]])
SEa = as.data.frame(olf1[["SEa"]])

olf1b = cbind(UA, PA, area, SEua, SEpa, SEa)
olf1b$class = c(1,10,14,22,26,100)

mtx1 = as.data.frame(olf1[["matrix"]])

#write.csv(olf1b, file = "Results_Olofsson_lvl1_V4.csv", row.names = F)
#write.csv(mtx1, file = "Matrix_Olofsson_lvl1_V4.csv", row.names = T)
#write.csv(nclass1, file = "MB_AM_class_area_lvl1_V4.csv", row.names = F)




