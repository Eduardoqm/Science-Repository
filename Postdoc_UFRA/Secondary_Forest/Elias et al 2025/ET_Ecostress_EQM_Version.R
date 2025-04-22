
#Evapotranspiration by ECOSTRESS (Elias Paper)

#Adapted from Divino V Silverio by Eduardo Q Marques 21-04-2025

library(tidyverse)
library(terra)
library(sf)

#Load Data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serra (Elias et al)/Analises_Elias/Rasters/ECOSTRESS")

guama=read_sf("G:/Meu Drive/Postdoc_UFRA/Papers/Serra (Elias et al)/Analises_Elias/Shapes/BR_Amazon_DrySeason_filtered.shp")
guama2=st_transform(guama,crs = 4326)
plot(guama2)

#fl=dir(pattern = "ECO3*")
ex=rast('ECO3ETPTJPL.001_EVAPOTRANSPIRATION_PT_JPL_ETcanopy_doy2022198192703_aid0009.tif')
ex2 <- rast(ext(guama2), resolution=res(ex))
res(ex)

#Get file names by month -------------------------------------------------------
metalist = list.files(path = "G:/Meu Drive/Postdoc_UFRA/Papers/Serra (Elias et al)/Analises_Elias/Rasters/ECOSTRESS/metadata", full.names = T)

#Read the file
meta1 = c(
Jan <- substr(readLines(metalist[1]), 127, 205),
Fev <- substr(readLines(metalist[2]), 127, 205),
Mar <- substr(readLines(metalist[3]), 127, 205),
Apr <- substr(readLines(metalist[4]), 127, 205))
head(meta1)

meta2 = c(
  May <- substr(readLines(metalist[5]), 127, 205),
  Jun <- substr(readLines(metalist[6]), 127, 205),
  Jul <- substr(readLines(metalist[7]), 127, 205),
  Agu <- substr(readLines(metalist[8]), 127, 205),
  Sep <- substr(readLines(metalist[9]), 127, 205))
head(meta2)

meta3 = c(
  Out <- substr(readLines(metalist[10]), 127, 205),
  Nov <- substr(readLines(metalist[11]), 127, 205),
  Dec <- substr(readLines(metalist[12]), 127, 205))
head(meta3)

#January to April --------------------------------------------------------------
Jan_Apr <- list()

for(i in 1:length(meta1)) {
  cat(i,'\n')
  r <-terra::rast(meta1[i])
  r2=terra::resample(r,ex2)
  Jan_Apr[[i]] <-r2
}


fl2=sprc(Jan_Apr)
fl3=mosaic(fl2,fun='max')
et2=max(fl3,na.rm = T)
plot(et2)

writeRaster(et2, "ECOSTRESS_EVAP_Jan_April_2022.tif")

#May to September --------------------------------------------------------------
May_Sep <- list()

for(i in 1:length(meta2)) {
  cat(i,'\n')
  r <-terra::rast(meta2[i])
  r2=terra::resample(r,ex2)
  May_Sep[[i]] <-r2
}


fl2=sprc(May_Sep)
fl3=mosaic(fl2,fun='max')
et3=max(fl3,na.rm = T)
plot(et3)

writeRaster(et3, "ECOSTRESS_EVAP_May_Sep_2022.tif")

#October to December -----------------------------------------------------------
Oct_Dec <- list()

for(i in 1:length(meta3)) {
  cat(i,'\n')
  r <-terra::rast(meta3[i])
  r2=terra::resample(r,ex2)
  Oct_Dec[[i]] <-r2
}


fl2=sprc(Oct_Dec)
fl3=mosaic(fl2,fun='max')
et4=max(fl3,na.rm = T)
plot(et4)

writeRaster(et4, "ECOSTRESS_EVAP_Oct_Dec_2022.tif")




# ------------------------------------------------------------------------------

#xx=do.call('rbind',results)
#max(xx$nrow) # 4275
#max(xx$ncol) # 4718

#et_all<- rast(results)
#et2=max(et_all,na.rm = T)
#et2= app(et_all, function(i) max(i,na.rm=T))

dir()

##
sf=rast("G:/Meu Drive/Postdoc_UFRA/Papers/Serra (Elias et al)/Analises_Elias/Rasters/MB_Forest_age_30m.tif")
plot(sf)
names(sf)="age"
etx=terra::resample(et2,sf)
plot(etx)

etx2=c(sf,etx)
res=zonal(etx, sf, "mean", na.rm=TRUE)
res$mm_m2=(res$max*0.0864)*0.408

#1 W m-2 = 0.0864 MJ m-2 day-1
#1 MJ m-2 day-1 = 0.408 mm day-1




# grafico de resultados - individuos
ggplot(res,aes(age,mm_m2))+
  geom_point()+
  stat_smooth(span=0.5)
##

##### analise estatistica
m_year=lm(log(LST_c)~log(sf_age),data=df)
summary(m0)
df$lst_year=exp(predict(m_year))
m_wet=lm(log(LST_c_wet)~log(sf_age),data=df)
summary(m_wet)
df$lst_wet=exp(predict(m_wet))
m_dry=lm(log(LST_c_dry)~log(sf_age),data=df)
summary(m_dry)
df$lst_dry=exp(predict(m_dry))



ggplot(df)+
  geom_point(aes(sf_age,LST_c))+
  geom_line(aes(sf_age,lst_year),colour='red')+
  labs(x='Secondary forests age (year)',
       y='Landsurface temperature (ºC)')+
  theme_minimal()+
  geom_label(aes( x=25, y=34,color='red',
                  label="LST_year r²adj=0.85"),
             size=4)
  
ggplot(df)+
  geom_point(aes(sf_age,LST_c_dry),colour='red')+
  geom_point(aes(sf_age,LST_c_wet),colour='blue')+
  geom_line(aes(sf_age,lst_dry),colour='red')+
  geom_line(aes(sf_age,lst_wet),colour='blue')+
    labs(x='Secondary forests age (year)',
       y='Landsurface temperature (ºC)')+
  theme_minimal()+
  geom_label(aes( x=25, y=33.5,
              label="LST_dry r²adj=0.82"),
             color='red',size=4)+
  geom_label(aes( x=25, y=31,
              label="LST_wet r²adj=0.61"),
             color='blue',size=4)


