#Vegetation hight vs Optimal CHI

#Eduardo Q Marques 05-10-2022

library(raster)

#Essential functions to VSCode --------------------------------------------------
plotx = function(w) {
    setwd("/home/queirozm/eqm_eth_ufz/Temp_maps")
    png(paste0(deparse(substitute(w)), ".png"))
    plot(w)
    dev.off()
}

#Load data ----------------------------------------------------------------------
setwd("/home/queirozm/eqm_eth_ufz/Data/Mapbiomas_GEDI/")
gedi = raster("GEDI_025_xingu_forest_2019.tif")
plotx(gedi)


setwd("/home/queirozm/eqm_eth_ufz/Data/DataFrames/")
gedi_mean = read.csv("GEDI_mean_pixel.csv")
colnames(gedi_mean) = c("ID", "Higth", "x", "y")


setwd("/home/queirozm/eqm_eth_ufz/Data/DataFrames/")
opt_chi = read.csv("Optimal_ERA5_Block_1979_2020.csv")


#GEDI to dataframe --------------------------------------------------------------
gedi_df = as.data.frame(gedi, xy = T)
colnames(gedi_df) = c("x", "y", "Higth")

library(tidyverse)
library(reshape2)


opt_chi2 = opt_chi %>%
        group_by(x, y)%>%
        slice(which.max(CHI))%>%
        unite("xy", x:y)%>%
        ungroup()

#setwd("/home/queirozm/eqm_eth_ufz/Data/DataFrames/CHIs/")
#write.csv(opt_chi2, "Optimal_CHI_ERA5_1979_2020.csv", row.names = F)

gedi_df2 = gedi_df %>% unite("xy", x:y)
gedi_mean = gedi_mean %>% unite("xy", x:y)#

df = full_join(opt_chi2, gedi_df2, id = "xy")
df = df %>% na.omit()

df_mean = full_join(opt_chi2, gedi_mean, id = "xy")
df_mean = df_mean %>% na.omit()#


library(ggplot2)
library(ggpubr)

a = ggplot(df, aes(x=CHI, y=Higth))+
        geom_point()+
        stat_cor(show.legend = F)+
        ggtitle("Resampled GEDI (mean) x CHI (1979 - 2020)")+
        geom_smooth()

ggsave(filename = "Xingu_OptCHI_Veg_resampled_Hight_1979_2020.png", plot = a,
  path = "/home/queirozm/eqm_eth_ufz/Temp_figures",
  width = 10, height = 10, units = "cm", dpi = 300)



b = ggplot(df_mean, aes(x=CHI, y=Higth))+
        geom_point()+
        stat_cor(show.legend = F)+
        ggtitle("GEDI (mean) x CHI (1979 - 2020)")+
        geom_smooth()

ggsave(filename = "Xingu_OptCHI_Veg_mean_Hight_1979_2020.png", plot = b,
  path = "/home/queirozm/eqm_eth_ufz/Temp_figures",
  width = 10, height = 10, units = "cm", dpi = 300)


#Plot CHI Maps
library(rgdal)
library(viridis)
pont = readOGR(dsn = "/home/queirozm/eqm_eth_ufz/Data/Shapes", layer = "Area1_ponto") #Area-1 point
pont2 = as.data.frame(pont)
colnames(pont2) = c("id","x","y")

df_mean2 = df_mean %>% separate(xy, c("x","y"), sep = "_")

c_class = ggplot(df_mean2)+
      geom_raster(aes(x, y, fill = Class))+
      coord_fixed()+
      #geom_point(data = pont2, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
      scale_fill_manual(values = c('#bdd7e7','#6baed6','#3182bd','#08519c','#bae4b3','#74c476','#238b45','#006d2c','#fcae91','#fb6a4a','#cb181d','#a50f15'))+
      labs(x=NULL, y=NULL, title = "Optimal Windowns")+
      theme_minimal()+
      theme(text = element_text(size = 14))

ggsave(filename = "Xingu_Optimal_CHIMap_classes_1979_2020.png", plot = c_class,
  path = "/home/queirozm/eqm_eth_ufz/Temp_figures",
  width = 15, height = 20, units = "cm", dpi = 300)


c_value = ggplot(df_mean2)+
      geom_raster(aes(x, y, fill = CHI))+
      coord_fixed()+
      #geom_point(data = pont2, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
      scale_fill_viridis(option = "magma", direction = -1, limits=c(0, 0.30), name = "Chi q = 0.9")+
      labs(x=NULL, y=NULL, title = "Optimal CHI")+
      theme_minimal()+
      theme(text = element_text(size = 14))

ggsave(filename = "Xingu_Optimal_CHIMap_values_1979_2020.png", plot = c_value,
  path = "/home/queirozm/eqm_eth_ufz/Temp_figures",
  width = 15, height = 20, units = "cm", dpi = 300)