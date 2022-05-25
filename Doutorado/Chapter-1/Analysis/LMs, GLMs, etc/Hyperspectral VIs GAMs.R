####################################
# Hyperspectral VIs GAMs 
#
# Eduardo Q Marques 25-05-2022     
####################################

library(tidyverse)
library(reshape2)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)
windowsFonts("Times New Roman" = windowsFont("Times New Roman"))

#Vegetation Indices ======================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

hy = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

#Modify and Filter Hyperion data ==========================================================
hy$year = as.numeric(hy$year)
hy$index = as.character(hy$index)
hy$treat = as.character(hy$treat)

hy$index[hy$index == "evi2"] <- c("EVI")
hy$index[hy$index == "ndvi"] <- c("NDVI")
hy$index[hy$index == "ndii"] <- c("NDII")
hy$index[hy$index == "vig"] <- c("VIG")
hy$index[hy$index == "vari"] <- c("VARI")
hy$index[hy$index == "nirv"] <- c("NIRv")
hy$index[hy$index == "lwvi2"] <- c("LWVI2")
hy$index[hy$index == "msi"] <- c("MSI")
hy$index[hy$index == "ndwi"] <- c("NDWI")
hy$index[hy$index == "pssr"] <- c("PSSR")
hy$index[hy$index == "psri"] <- c("PSRI")
hy$index[hy$index == "sipi"] <- c("SIPI")
hy$index[hy$index == "wbi"] <- c("WBI")
hy$index[hy$index == "pri"] <- c("PRI")
hy$index[hy$index == "rendvi"] <- c("RENDVI")
hy$index[hy$index == "nbr"] <- c("NBR")
hy$index[hy$index == "nbr2"] <- c("NBR2")

hy$treat[hy$treat == "control"] <- c("Control")
hy$treat[hy$treat == "b3yr"] <- c("B3yr")
hy$treat[hy$treat == "b1yr"] <- c("B1yr")


hy = hy %>% filter(index %in% c("VIG","VARI","PSSR","MSI","PSRI"))


#Edge - Core separation
diffy = min(hy$y) - max(hy$y)
hy$dist = ((max(hy$y) - hy$y)/diffy)*1000
hy$dist = abs(hy$dist)
summary(hy$dist)

hy$dist2 = c("a")
hy$dist2[hy$dist <= 250] = c("Edge")
hy$dist2[hy$dist > 250] = c("Interior")

colnames(hy) = c("id1","id2","x","y","layer","Indice","Year","Valor","Treatment","Dist","Dist2")

#Join VIs and field data =================================================================
hy = hy %>% 
  na.omit() %>% 
  #filter(Valor < 0.3) %>% 
  select("Indice","Year","Treatment","Valor","Dist2")

colnames(hy) = c("Variavel","Year","Treatment","Valor","Dist")

#Plot data =================================================================================
eqm = c("orange", "red", "blue") #My color palette

hy2 = hy
hy2$Variavel <- factor(hy2$Variavel,      # Reordering group factor levels
                       levels = c("PSRI","MSI","PSSR","VARI","VIG"))


#Smooth time series
panel = ggplot(hy2, aes(x=Year, y=Valor, color = Treatment))+
  geom_smooth(aes(group=Treatment), alpha = 0.3, size = 1, method = "gam", formula = y ~ poly(x, 4))+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Treatment, shape = Treatment))+
  facet_grid(rows = vars(Variavel), cols = vars(Dist), scales = "free")+
  scale_color_manual(values = eqm)+
  xlim(2004, 2012)+
  theme_minimal()+
  #labs(title = "                          Edge                                                    Interior",
   #    y = "VIG                                      MSI                                    RENDVI                             NBR2",
    #   x = "Year")+
  labs(y = NULL)+
  theme(text = element_text(family = "Times New Roman", size = 14),
        panel.background = element_rect(colour = "gray", size=1),
        axis.text.x = element_text(angle = 45),
        legend.position = c(30,30)); panel


ggsave(filename = "Hyperpectral_GAM_smooth.png", plot = panel,
    path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Models",
    #width = 20, height = 25, units =  "cm", dpi = 300)
    width = 14, height = 20, units =  "cm", dpi = 300)






































