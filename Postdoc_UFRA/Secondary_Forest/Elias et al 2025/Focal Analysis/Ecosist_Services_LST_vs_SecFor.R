#LST by Secondary Forest Age (Focal)

#Eduardo Q Marques 22-08-2025

library(terra)
library(tidyverse)
library(sf)
library(future) #Multicore work

parallel::detectCores()

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/Resampled_70m")
dir()

#LST
lst_year = rast("LST_Landsat_Annual_2022_2023_70m.tif")
lst_dry = rast("LST_Landsat_Dry_2022_2023_70m.tif")
lst_wet = rast("LST_Landsat_Wet_2022_2023_70m.tif")

plot(lst_year)
plot(lst_dry)
plot(lst_wet)

#Secondary Forest
sf=rast("MB_Forest_age_70m.tif")
plot(sf)

#Primary Forest
fr=rast("Forest_70m.tif")
plot(fr)

focal_sf = function(x){
  #Calculating LST for Primary Forest --------------------------------------------
  fr_pri=ifel(is.na(sf),fr,NA) #Selecting only Primary Forest
  #plot(fr_pri)
  
  lst_pri=ifel(is.na(fr_pri),NA,x)
  #plot(lst_pri)
  
  #Use focal to calculate difference in Secondary and Primary (Delta)-------------
  lst_f <- focal(lst_pri, w=61, median, na.rm=TRUE)
  #plot(lst_f)
  
  lst_delta_pri=ifel(is.na(sf),NA,x-lst_f)
  #plot(lst_delta_pri)
  
  resf=as.data.frame(c(sf,lst_delta_pri))
  colnames(resf) = c("sf_age", "lst")
  #head(resf)
  

  resf2=resf%>%
    mutate(sf_age=round(sf_age,0))%>%
    group_by(sf_age)%>%
    summarise(lst=mean(lst,na.rm=T))
  #head(resf2)
  return(resf2)
}


plan(multisession, workers = 27)
df1 = focal_sf(lst_year); df1$cond = "Annual"
df2 = focal_sf(lst_dry); df2$cond = "Dry Season"
df3 = focal_sf(lst_wet); df3$cond = "Rainy Season"
lst_df = rbind(df1, df2, df3)

gglst = ggplot(lst_df,aes(x=sf_age, y=lst, col=cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Secondary forest age (year)",y="Δ LST", col = "Condition")+
  theme_minimal(); gglst


setwd("G:\\Meu Drive\\Postdoc_UFRA\\Papers\\Serrapilheira (Elias et al)\\Analises_Elias\\Figures\\")
ggsave(plot = gglst, "Delta_LST_Amazonia_full_W61.png", dpi = 300,
       height = 10, width = 15, units = "cm")

setwd("G:\\Meu Drive\\Postdoc_UFRA\\Papers\\Serrapilheira (Elias et al)\\Analises_Elias\\Dados\\")
write.csv(lst_df, "LST_SecFor_Age_W61.csv", row.names = F)

