# Graphics for CHI values in each pixels of Xingu for every combination of Time and Space window

# Eduardo Q Marques  11-08-2022

library(ggplot2)
library(ggpubr)
library(viridis)
library(rgdal)

setwd("/home/queirozm/eqm_eth_ufz/Data")
pont = readOGR(dsn = "Shapes", layer = "Area1_ponto") #Area-1 point

setwd("/home/queirozm/eqm_eth_ufz/Data/DataFrames/CHIs")
p1 = read.csv("CHI_Xingu_1p.csv", sep = ",")
p3 = read.csv("CHI_Xingu_3p.csv", sep = ",")
p5 = read.csv("CHI_Xingu_5p.csv", sep = ",")
p7 = read.csv("CHI_Xingu_7p.csv", sep = ",")

pont2 = as.data.frame(pont)
colnames(pont2) = c("id","x","y")

chiplt = function(z, k, w) {
  b = ggplot()+
        geom_raster(data = z, aes(x, y, fill = CHI))+
        coord_fixed()+
        geom_point(data = pont2, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
        scale_fill_viridis(option = "magma", direction = -1, limits=c(0, 0.36), name = "Chi q = 0.9")+
        labs(x=NULL, y=NULL, title = k)+
        facet_wrap(~Days, nrow = 1)+
        theme_minimal()+
        theme(text = element_text(size = 14))

  ggsave(filename = w, plot = b,
    path = "/home/queirozm/eqm_eth_ufz/Temp_figures/Result_maps",
    width = 37, height = 20, units = "cm", dpi = 300)
}

chiplt(p1, "Tail Dependence (0.25°)", "Xingu_CHIMap_1p.png")
chiplt(p3, "Tail Dependence (0.75°)", "Xingu_CHIMap_3p.png")
chiplt(p5, "Tail Dependence (1.25°)", "Xingu_CHIMap_5p.png")
chiplt(p7, "Tail Dependence (1.75°)", "Xingu_CHIMap_7p.png")

#Optimal Chi Value and window -----------------------------------------
library(tidyverse)
library(reshape2)

p1$degrees = '0.25°'
p3$degrees = '0.75°'
p5$degrees = '1.25°'
p7$degrees = '1.75°'

df = rbind(p1, p3, p5, p7)

#Creating classes
df2 = df %>%
  group_by(x, y) %>%
  #summarize(CHI = max(CHI)) %>%
  slice(which.max(CHI))%>%
  unite("Class", Days, degrees, sep = ", ") %>%
  ungroup()
#write.csv(df2, "Optimal_CHI_ERA5.csv", row.names = F)


c_value = ggplot(df2)+
      geom_raster(aes(x, y, fill = CHI))+
      coord_fixed()+
      geom_point(data = pont2, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
      scale_fill_viridis(option = "magma", direction = -1, limits=c(0, 0.36), name = "Chi q = 0.9")+
      labs(x=NULL, y=NULL, title = "Optimal CHI")+
      theme_minimal()+
      theme(text = element_text(size = 14))

ggsave(filename = "Xingu_Optimal_CHIMap_values.png", plot = c_value,
  path = "/home/queirozm/eqm_eth_ufz/Temp_figures/Result_maps",
  width = 15, height = 20, units = "cm", dpi = 300)


c_class = ggplot(df2)+
      geom_raster(aes(x, y, fill = Class))+
      coord_fixed()+
      geom_point(data = pont2, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
      scale_fill_manual(values = c('#08519c','#bae4b3','#74c476','#238b45','#006d2c','#fcae91','#fb6a4a','#cb181d','#a50f15'))+
      labs(x=NULL, y=NULL, title = "Optimal Windowns")+
      theme_minimal()+
      theme(text = element_text(size = 14))

ggsave(filename = "Xingu_Optimal_CHIMap_classes.png", plot = c_class,
  path = "/home/queirozm/eqm_eth_ufz/Temp_figures/Result_maps",
  width = 15, height = 20, units = "cm", dpi = 300)


class_count = df2 %>% count(Class); class_count #Count how much pixels of each class
class_count$perc = (class_count$n*100)/237