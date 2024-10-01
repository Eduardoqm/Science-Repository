#Amazonian Validations Analysis - Hit and errors of classifications

#Eduardo Q Marques 26-09-2024

library(tidyverse)
library(ggplot2)
#library(ggalluvial)

#Load Classes Data -------------------------------------------------------------
setwd("G:/My Drive/Postdoc_UFRA/Papers/Amazonia_validation (Marques et al)/Shapes")
dir()

df = read.csv("Validation_Classes_Result.csv")
df = df[,-6] #Exclude level 4 from MapBiomas

#Converto to Not Observed class
df$Val_lv3[df$Val_lv3 == 100] = 27
df$Val_lv2[df$Val_lv2 == 100] = 27
df$Val_lv1[df$Val_lv1 == 100] = 27

df2 = df %>%
  filter(MB_lv3 != 0) %>% #Remove years that dont match
  filter(Name_MB != "indefinifo") %>% #Remove no classified points
  na.omit()

df2$Val_lv1 = as.numeric(df2$Val_lv1)
df2$Val_lv2 = as.numeric(df2$Val_lv2)
df2$Val_lv3 = as.numeric(df2$Val_lv3)

#Level 1 -----------------------------------------------------------------------
l1 = df2[,c(5,8)]

list_mb = unique(l1$MB_lv1) #List of MapBiomes classes

#Extract frequency validate point by each MapBiomes Class
l1b = l1 %>% 
  group_by(Val_lv1) %>% 
  filter(MB_lv1 == 1) %>% 
  summarise(freq = table(Val_lv1))

l1b$perc = 0 - (l1b$freq/sum(l1b$freq)) #Input negative values to != class
l1b$mb = list_mb[1]

for (z in 2:length(list_mb)) {
  l1x = l1 %>% 
    group_by(Val_lv1) %>% 
    filter(MB_lv1 == list_mb[z]) %>% 
    summarise(freq = table(Val_lv1))
  
  l1x$perc = 0 - (l1x$freq/sum(l1x$freq))
  l1x$mb = list_mb[z]
  
  l1b = rbind(l1b, l1x)
}

#Names of classes
#Validation
l1b$Val_lv1[l1b$Val_lv1 == 1] = c("Forest")
l1b$Val_lv1[l1b$Val_lv1 == 10] = c("Herbaceous and S. Veg.")
l1b$Val_lv1[l1b$Val_lv1 == 14] = c("Farming")
l1b$Val_lv1[l1b$Val_lv1 == 22] = c("Non vegetated area")
l1b$Val_lv1[l1b$Val_lv1 == 26] = c("Water")
l1b$Val_lv1[l1b$Val_lv1 == 27] = c("Not Observed")

#MapBiomes
l1b$mb[l1b$mb == 1] = c("Forest")
l1b$mb[l1b$mb == 10] = c("Herbaceous and S. Veg.")
l1b$mb[l1b$mb == 14] = c("Farming")
l1b$mb[l1b$mb == 22] = c("Non vegetated area")
l1b$mb[l1b$mb == 26] = c("Water")
l1b$mb[l1b$mb == 27] = c("Not Observed")
  
#Convert equivalent class to positive
list_cls = unique(l1b$Val_lv1)

l1p = l1b
l1p$perc = abs(l1p$perc)

for (z in 1:length(list_cls)) {
  l1b$perc[l1b$mb == list_cls[z] & l1b$Val_lv1 == list_cls[z]] <- (l1p$perc[l1p$mb == list_cls[z] & l1p$Val_lv1 == list_cls[z]])
}

#Graphics ----------------------------------------------------------------------
ggplot(l1b, aes(x = perc, y = reorder(mb,(perc > 0.9)), fill = Val_lv1))+
  geom_bar(position = "stack", stat = "identity")+
  labs(x = "Percentage of class",
       y = "MapBiomes Class",
       fill = "Validate Class")+
  geom_vline(xintercept = 0, color = "black", size = 1, linetype = "dashed")+
  scale_fill_manual(values = c("#FFFFB2", "#1f8d49", "#ad975a",
                               "#d4271e", "gray", "#0000FF"))+
  theme_minimal()



#Level 3 -----------------------------------------------------------------------
l3 = df2[,c(3,6)]

#list_mb = unique(l3$MB_lv3) #List of MapBiomes classes
list_mb = unique(l3$Val_lv3) #List of MapBiomes classes

#Extract frequency validate point by each MapBiomes Class
l3b = l3 %>% 
  group_by(MB_lv3) %>% 
  filter(Val_lv3 == 3) %>% 
  summarise(freq = table(MB_lv3))

l3b$perc = 0 - (l3b$freq/sum(l3b$freq)) #Input negative values to != class
l3b$mb = list_mb[1]

for (z in 2:length(list_mb)) {
  l3x = l3 %>% 
    group_by(MB_lv3) %>% 
    filter(Val_lv3 == list_mb[z]) %>% 
    summarise(freq = table(MB_lv3))
  
  l3x$perc = 0 - (l3x$freq/sum(l3x$freq))
  l3x$mb = list_mb[z]
  
  l3b = rbind(l3b, l3x)
}

#Names of classes
colnames(l3b)[1] = c("Val_lv3")

#Validation
l3b$Val_lv3[l3b$Val_lv3 == 3] = c("Forest Formation")
l3b$Val_lv3[l3b$Val_lv3 == 4] = c("Savanna Formation")
l3b$Val_lv3[l3b$Val_lv3 == 5] = c("Mangrove")
l3b$Val_lv3[l3b$Val_lv3 == 6] = c("Floodable Forest")
l3b$Val_lv3[l3b$Val_lv3 == 9] = c("Forest Plantation")
l3b$Val_lv3[l3b$Val_lv3 == 11] = c("Wetland")
l3b$Val_lv3[l3b$Val_lv3 == 12] = c("Grassland")
l3b$Val_lv3[l3b$Val_lv3 == 13] = c("Herbaceous Sandbank Vegetation") #class 50 in MB8
l3b$Val_lv3[l3b$Val_lv3 == 15] = c("Pasture")
l3b$Val_lv3[l3b$Val_lv3 == 19] = c("Temporary Crop")
l3b$Val_lv3[l3b$Val_lv3 == 23] = c("Beach, Dune and Sand Spot")
l3b$Val_lv3[l3b$Val_lv3 == 24] = c("Urban Area")
l3b$Val_lv3[l3b$Val_lv3 == 25] = c("Other non Vegetated Areas")
l3b$Val_lv3[l3b$Val_lv3 == 27] = c("Not Observed")
l3b$Val_lv3[l3b$Val_lv3 == 29] = c("Rocky Outcrop")
l3b$Val_lv3[l3b$Val_lv3 == 30] = c("Mining")
l3b$Val_lv3[l3b$Val_lv3 == 32] = c("Hypersaline Tidal Flat")
l3b$Val_lv3[l3b$Val_lv3 == 33] = c("River, Lake and Ocean")
l3b$Val_lv3[l3b$Val_lv3 == 36] = c("Perennial Crop")

#MapBiomes
l3b$mb[l3b$mb == 3] = c("Forest Formation")
l3b$mb[l3b$mb == 4] = c("Savanna Formation")
l3b$mb[l3b$mb == 5] = c("Mangrove")
l3b$mb[l3b$mb == 6] = c("Floodable Forest")
l3b$mb[l3b$mb == 9] = c("Forest Plantation")
l3b$mb[l3b$mb == 11] = c("Wetland")
l3b$mb[l3b$mb == 12] = c("Grassland")
l3b$mb[l3b$mb == 13] = c("Herbaceous Sandbank Vegetation") #class 50 in MB8
l3b$mb[l3b$mb == 15] = c("Pasture")
l3b$mb[l3b$mb == 19] = c("Temporary Crop")
l3b$mb[l3b$mb == 23] = c("Beach, Dune and Sand Spot")
l3b$mb[l3b$mb == 24] = c("Urban Area")
l3b$mb[l3b$mb == 25] = c("Other non Vegetated Areas")
l3b$mb[l3b$mb == 27] = c("Not Observed")
l3b$mb[l3b$mb == 29] = c("Rocky Outcrop")
l3b$mb[l3b$mb == 30] = c("Mining")
l3b$mb[l3b$mb == 32] = c("Hypersaline Tidal Flat")
l3b$mb[l3b$mb == 33] = c("River, Lake and Ocean")
l3b$mb[l3b$mb == 36] = c("Perennial Crop")

#Convert equivalent class to positive
list_cls = unique(l3b$Val_lv3)

l3p = l3b
l3p$perc = abs(l3p$perc)

for (z in 1:length(list_cls)) {
  l3b$perc[l3b$mb == list_cls[z] & l3b$Val_lv3 == list_cls[z]] <- (l3p$perc[l3p$mb == list_cls[z] & l3p$Val_lv3 == list_cls[z]])
}


l3b$ord = l3b$perc
l3b$ord[l3b$ord <= 0] <- 0

l3b$ord2 = l3b$perc
l3b$ord2[l3b$ord2 < 0] <- NA



#l3b = l3b %>% 
#  mutate(mb = factor(mb, levels = c()))


#Graphics ----------------------------------------------------------------------
plt1 = ggplot(l3b, aes(x = perc, y = reorder(mb, ord, sum), fill = Val_lv3))+
  geom_bar(position = "stack", stat = "identity")+
  labs(x = "Percentage of class",
       y = "Validate Class",
       fill = "MapBiomes Class")+
  geom_vline(xintercept = 0, color = "black", size = 1, linetype = "dashed")+
  #geom_text(aes(label = paste0(ord2*100, "%")),
  #          position=position_dodge(width=0.9),
  #          vjust=-0.25)+
  
  scale_fill_manual(values = c("#026975","#1f8d49","#7a5900","#d6bc74","#fc8114",
                               "#04381d","#9c0027","#edde6e","#d042de","#2532e4",
                               "#ffaa5f","#7dc975","#C27BA0","#d4271e","#519799"))+
  
  theme_minimal(); plt1


#Saving
setwd("G:/My Drive/Postdoc_UFRA/Papers/Amazonia_validation (Marques et al)/Figures")

ggsave(filename = "Percent_Class.tiff", plot = plt1,
       width = 19, height = 13, units = "cm", dpi = 300)

###
















#Sankey (Alluvial) - https://r-charts.com/flow/ggalluvial/
ggplot(data = l1b, aes(axis1 = Val_lv1, axis2 = mb, y = freq))+
  geom_alluvium(aes(fill = Val_lv1), alpha = 0.5)+
  #geom_stratum(aes(fill = Val_lv1))+
  geom_stratum(col = "black", alpha = 0.2)+
  #geom_text(stat = "stratum", aes(label = after_stat(stratum)))+
  scale_x_discrete(limits = c("Validation", "MapBiomes"), expand = c(0.1, 0.1))+
  scale_fill_manual(values = c("#FFFFB2", "#1f8d49", "#ad975a",
                               "#d4271e", "gray", "#0000FF"))+
  labs(title = "     Level 1", fill = NULL)+
  theme_void()+
  theme(axis.title.x = element_text(),
        axis.text.x = element_text())






# install.packages("circlize")
library(circlize)

l1c = l1

#Names of classes
#Validation
l1c$Val_lv1[l1c$Val_lv1 == 1] = c("Forest")
l1c$Val_lv1[l1c$Val_lv1 == 10] = c("Herbaceous and Shrubby Vegetation")
l1c$Val_lv1[l1c$Val_lv1 == 14] = c("Farming")
l1c$Val_lv1[l1c$Val_lv1 == 22] = c("Non vegetated area")
l1c$Val_lv1[l1c$Val_lv1 == 26] = c("Water")
l1c$Val_lv1[l1c$Val_lv1 == 27] = c("Not Observed")

#MapBiomes
l1c$MB_lv1[l1c$MB_lv1 == 1] = c("Forest")
l1c$MB_lv1[l1c$MB_lv1 == 10] = c("Herbaceous and Shrubby Vegetation")
l1c$MB_lv1[l1c$MB_lv1 == 14] = c("Farming")
l1c$MB_lv1[l1c$MB_lv1 == 22] = c("Non vegetated area")
l1c$MB_lv1[l1c$MB_lv1 == 26] = c("Water")
l1c$MB_lv1[l1c$MB_lv1 == 27] = c("Not Observed")


chordDiagram(l1c, column.col = c("#FFFFB2", "#1f8d49", "#ad975a",
                                 "#d4271e", "gray", "#0000FF"),
             annotationTrack =  c("name"))


circos.clear()

###


