#Evapotranspiration by Secondary Forest Age (Focal)

#Eduardo Q Marques 27-08-2025

library(terra)
library(tidyverse)
library(sf)
library(future) #Multicore work

parallel::detectCores()

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/Resampled_70m")
dir()

#ECOSTRESS
evp_year = rast("ECOSTRESS_EVAP_Annual_2022_70m.tif")
evp_dry = rast("ECOSTRESS_EVAP_DrySeason_2022_70m.tif")
evp_wet = rast("ECOSTRESS_EVAP_WetSeason_2022_70m.tif")
plot(evp_year)
plot(evp_dry)
plot(evp_wet)

#Primary Forest
fr=rast("Forest_70m.tif")
plot(fr)

#Secondary Forest
sf=rast("MB_Forest_age_70m.tif")
plot(sf)

#Percentage of Secondary Forest
sf_perc = rast("Perc_SecForest_70m.tif")
plot(sf_perc)

#Proccess before Focal ---------------------------------------------------------
#Selecting only Primary Forest
fr_pri=ifel(is.na(sf),fr,NA)
plot(fr_pri)

#Selection Secondary forest > 30% purity
sf=ifel(sf_perc < 30,sf,NA)
plot(sf)

#Focal Function ----------------------------------------------------------------
focal_sf = function(x){
  #Calculating LST for Primary Forest
    evp_pri=ifel(is.na(fr_pri),NA,x)
  #plot(evp_pri)
  
  #Use focal to calculate difference in Secondary and Primary (Delta)-------------
  evp_f <- focal(evp_pri, w=61, median, na.rm=TRUE)
  #plot(evp_f)
  
  evp_delta_pri=ifel(is.na(sf),NA,x-evp_f)
  #plot(evp_delta_pri)
  
  resf=as.data.frame(c(sf,evp_delta_pri))
  colnames(resf) = c("sf_age", "et")
  #head(resf)
  
  resf2=resf%>%
    mutate(sf_age=round(sf_age,0))%>%
    group_by(sf_age)%>%
    summarise(et=mean(et,na.rm=T))
  #head(resf2)
  return(resf2)
}

#Running Focal function, plotting and saving results ---------------------------
plan(multisession, workers = 27) # 9:35 AM
df1 = focal_sf(evp_year); df1$cond = "Annual"
df2 = focal_sf(evp_dry); df2$cond = "Dry Season"
df3 = focal_sf(evp_wet); df3$cond = "Rainy Season"
evp_df = rbind(df1, df2, df3)

gglst = ggplot(evp_df,aes(x=sf_age, y=et, col=cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Secondary forest age (year)",y="Δ Evapotranspiration", col = "Condition")+
  theme_minimal(); gglst


setwd("G:\\Meu Drive\\Postdoc_UFRA\\Papers\\Serrapilheira (Elias et al)\\Analises_Elias\\Figures\\")
ggsave(plot = gglst, "Delta_ET_Amazonia_full_W61.png", dpi = 300,
       height = 10, width = 15, units = "cm")

setwd("G:\\Meu Drive\\Postdoc_UFRA\\Papers\\Serrapilheira (Elias et al)\\Analises_Elias\\Dados\\")
write.csv(lst_df, "ET_SecFor_Age_W61.csv", row.names = F)

