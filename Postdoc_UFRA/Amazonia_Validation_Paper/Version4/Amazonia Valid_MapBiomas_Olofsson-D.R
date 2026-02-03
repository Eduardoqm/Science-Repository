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
setwd("G:/My Drive/Research/Papers/Amazonia_validation (Marques et al)/Accuracy_Test")

UA = as.data.frame(olf3[["UA"]])
PA = as.data.frame(olf3[["PA"]])
area = as.data.frame(olf3[["area"]])
SEua = as.data.frame(olf3[["SEua"]])
SEpa = as.data.frame(olf3[["SEpa"]])
SEa = as.data.frame(olf3[["SEa"]])

olf3b = cbind(UA, PA, area, SEua, SEpa, SEa)
olf3b$class = c(3,4,5,6,9,11,12,15,19,21,23,24,25,29,30,31,32,33,36,100,27,13)

mtx3 = as.data.frame(olf3[["matrix"]])

write.csv(olf3b, file = "Results_Olofsson_lvl3_V4.csv", row.names = F)
write.csv(mtx3, file = "Matrix_Olofsson_lvl3_V4.csv", row.names = T)
write.csv(nclass3, file = "MB_AM_class_area_lvl3_V4.csv", row.names = F)

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
olf2b$class = c(3,4,5,6,9,11,12,15,18,21,23,24,25,29,30,31,32,33,100,27,13)

mtx2 = as.data.frame(olf2[["matrix"]])

write.csv(olf2b, file = "Results_Olofsson_lvl2_V4.csv", row.names = F)
write.csv(mtx2, file = "Matrix_Olofsson_lvl2_V4.csv", row.names = T)
write.csv(nclass2, file = "MB_AM_class_area_lvl2_V4.csv", row.names = F)

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
olf1b$class = c(1,10,14,22,26,100,27)

mtx1 = as.data.frame(olf1[["matrix"]])

write.csv(olf1b, file = "Results_Olofsson_lvl1_V4.csv", row.names = F)
write.csv(mtx1, file = "Matrix_Olofsson_lvl1_V4.csv", row.names = T)
write.csv(nclass1, file = "MB_AM_class_area_lvl1_V4.csv", row.names = F)















#Olofsson exemplos -------------------------------------------------------------

library(mapaccuracy)


## Example 1 in Olofsson et al. (2013)
r<-c(rep("1",102),rep("2",280),rep("3",118))
m<-c(rep("1",97) ,rep("2",3), rep("3",2),rep("2",279),
     "3",rep("1",3),rep("2",18),rep("3",97))
Nh<-c(22353, 1122543, 610228)
names(Nh)<-c("1", "2", "3")
a<-olofsson(r, m, Nh)

# compare to paper:
a$area[1]                                       # eq. 9
a$area[1]*sum(Nh)                               # eq. 10
a$SEa[1]*sum(Nh)                                # eq. 12
a$area[1]*sum(Nh)-qnorm(0.975)*a$SEa[1]*sum(Nh) # 95% CI lower bound (note typo in the paper)
a$area[1]*sum(Nh)+qnorm(0.975)*a$SEa[1]*sum(Nh) # 95% CI upper bound
a$UA[1]                                         # eq. 14
a$PA[1]                                         # eq. 15
a$OA                                            # eq. 16
a$UA                                            # table 4
qnorm(0.975)*a$SEua                             # table 4
a$PA                                            # table 4
qnorm(0.975)*a$SEpa                             # table 4
a$matrix                                        # table 4



## Example 2 in Olofsson et al. (2013)
r<-c(rep("1", 129), rep("2", 403), rep("3", 611))
m<-c(rep("1", 127), "2", "2", rep("1", 66), rep("2", 322), rep("3", 15), rep("1", 54),
     rep("2", 17), rep("3", 540))
Nh<-c(0.007, 0.295, 0.698)
names(Nh)<-c("1", "2", "3")
b<-olofsson(r, m, Nh)

# compare to paper (table 6):
b$OA
qnorm(0.975)*b$SEoa
b$UA
qnorm(0.975)*b$SEua
b$PA
qnorm(0.975)*b$SEpa



## Example of table 8 in Olofsson et al. (2014)
r<-c(rep(1,69),rep(2,56),rep(3,175),rep(4,340))
m<-c(rep(1,66), 3, rep(4,2), rep(2,55), 4, rep(1,5), rep(2,8),
     rep(3,153),rep(4,9),rep(1,4),rep(2,12),rep(3,11),rep(4,313))
r[r==1] <- m[m==1] <- "Deforestation"
r[r==2] <- m[m==2] <- "Forest gain"
r[r==3] <- m[m==3] <- "Stable forest"
r[r==4] <- m[m==4] <- "Stable non-forest"
Nh<-c("Deforestation"=200000, "Forest gain"=150000,
      "Stable forest"=3200000, "Stable non-forest"=6450000) * 30^2 # Landsat pixel area = 30^2
e<-olofsson(r, m, Nh)

# compare to paper, left-hand of p. 54:
e$UA                            # User's accuracy
qnorm(0.975)*e$SEua             # 95% CI width
e$PA                            # Producer's accuracy
qnorm(0.975)*e$SEpa             # 95% CI width
e$OA                            # Overall accuracy
qnorm(0.975)*e$SEoa             # 95% CI width

# compare to paper, right-hand of p. 54:
e$area[1]*sum(Nh)/10000                 # deforestation in hectares
qnorm(0.975)*e$SEa[1]*sum(Nh)/10000     # 95% CI width in hectares
e$area[2]*sum(Nh)/10000                 # forest gain in hectares
qnorm(0.975)*e$SEa[2]*sum(Nh)/10000     # 95% CI width in hectares
e$area[3]*sum(Nh)/10000                 # stable forest in hectares
qnorm(0.975)*e$SEa[3]*sum(Nh)/10000     # 95% CI width in hectares
e$area[4]*sum(Nh)/10000                 # stable non-forest in hectares
qnorm(0.975)*e$SEa[4]*sum(Nh)/10000     # 95% CI width in hectares


# change class order
olofsson(r, m, Nh[c(4,2,1,3)])


# m (map) may include classes not found in r (reference)
r<-c(rep("1",102),rep("2",280),rep("3",118))
m<-c(rep("1",97) ,rep("2",3), rep("3",2),rep("2",279),
     "3",rep("1",3),rep("2",18),rep("3",95), rep("4",2))
Nh<-c("1"=22353, "2"=1122543, "3"=610228, "4"=10)
olofsson(r, m, Nh)

# r (reference) may include classes not found in m (map)
r<-c(rep("1",102),rep("2",280),rep("3",116),rep("4",2))
m<-c(rep("1",97) ,rep("2",3), rep("3",2),rep("2",279),
     "3",rep("1",3),rep("2",18),rep("3",97))
Nh<-c("1"=22353, "2"=1122543, "3"=610228)
olofsson(r, m, Nh)

# can add classes not found neither in r nor m
Nh<-c(Nh, "9"=0)
olofsson(r, m, Nh)
