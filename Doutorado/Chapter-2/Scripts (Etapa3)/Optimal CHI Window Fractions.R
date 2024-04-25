############################################
# Optimal Temporal/Spatial Window Fraction #
#                                          #
# Eduardo Q Marques 25-04-2024             #
############################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(viridis)
library(plotly)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")

opt_chi = read.csv("Optimal_ERA5_Block_1979_2020.csv", sep = ",")

opt_chi2 = opt_chi %>%
  group_by(x, y) %>%
  slice(which.max(CHI))%>%
  ungroup()

opt_chi2$Class = as.character(opt_chi2$Class)

#Calculating Fraction of Classes -----------------------------------------------
opt_chi2$count = 1
frac = opt_chi2 %>% 
  group_by(Class) %>% 
  summarise(count = sum(count))

frac$perc = (frac$count*100)/336

frac$perc_d = c(3,3,3,3,13,13,13,13,84,84,84,84)
frac$perc_p = c(3,7,17,73,3,7,17,73,3,7,17,73)
frac$day = c("1d","1d","1d","1d","3d","3d","3d","3d","5d","5d","5d","5d")
frac$pixel = rep(c("0.25°","0.75°","1.25°","1.75°"), 3)

c = ggplot(frac, aes(x=perc, y=day, fill = Class))+
  geom_bar(position="stack", stat="identity")+
  #scale_fill_manual(values = c('#cccccc','#969696','#525252','#252525'))+
  scale_fill_manual(values = c('#bdd7e7','#6baed6','#3182bd','#08519c',
                               '#bae4b3','#74c476','#238b45','#006d2c',
                               '#fcae91','#fb6a4a','#cb181d','#a50f15'))+
  labs(x="Percentage", y = NULL, fill = NULL,
       title = "c)")+
  annotate(geom="text", x=84, y=3, label="84%", color="black")+
  annotate(geom="text", x=16, y=2, label="13%", color="black")+
  annotate(geom="text", x=6, y=1, label="3%", color="black")+
  xlim(0, 84)+
  theme_minimal()+
  theme(legend.position = c(100, 100)); c



ggsave(filename = "Class_Pixels_Fraction.tiff", plot = c,
       path = "C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 20, height = 9, units = "cm", dpi = 300)








