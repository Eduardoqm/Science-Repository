
#Evapotranspiration by ECOSTRESS (Elias Paper)

#Adapted from Divino V Silverio by Eduardo Q Marques 21-04-2025

library(tidyverse)
library(terra)
library(sf)
library(future) #Multicore work

parallel::detectCores()

#Load Data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/ECOSTRESS_day")

guama=read_sf("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Shapes/BR_Amazon_DrySeason_filtered.shp")
guama2=st_transform(guama,crs = 4326)
plot(guama2)

#fl=dir(pattern = "ECO3*")
#ex=rast('ECO3ETPTJPL.001_EVAPOTRANSPIRATION_PT_JPL_ETcanopy_doy2022198192703_aid0009.tif')
ex=rast("ECO_L3T_JET.002_ETdaily_doy2022002132231_aid0009_23S.tif")
ex2 <- rast(ext(guama2), resolution=res(ex))
res(ex)

#Get file names by month -------------------------------------------------------
metalist = list.files(path = "G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/ECOSTRESS_day/metadata", full.names = T)

#Read the file
meta1 = c(
Jan <- substr(readLines(metalist[1]), 127, 205),
Fev <- substr(readLines(metalist[2]), 127, 205),
Mar <- substr(readLines(metalist[3]), 127, 205),
Apr <- substr(readLines(metalist[4]), 127, 205))
head(meta1); length(meta1)

meta2 = c(
  May <- substr(readLines(metalist[5]), 127, 205),
  Jun <- substr(readLines(metalist[6]), 127, 205),
  Jul <- substr(readLines(metalist[7]), 127, 205))
head(meta2); length(meta2)

meta3 = c(
  Agu <- substr(readLines(metalist[8]), 127, 205),
  Sep <- substr(readLines(metalist[9]), 127, 205))
head(meta3); length(meta3)

meta4 = c(
  Out <- substr(readLines(metalist[10]), 127, 205),
  Nov <- substr(readLines(metalist[11]), 127, 205),
  Dec <- substr(readLines(metalist[12]), 127, 205))
head(meta4); length(meta4)

#January to April --------------------------------------------------------------
Jan_Apr <- list()

#for(i in 1:length(meta1)) {
#  cat(i,'\n')
#  r <-terra::rast(meta1[i])
#  r2=terra::resample(r,ex2)
#  Jan_Apr[[i]] <-r2
#}

#fl2=sprc(Jan_Apr)
#fl3=mosaic(fl2,fun='max')
#et2=max(fl3,na.rm = T)
#plot(et2)

#writeRaster(et2, "ECOSTRESS_EVAP_Jan_April_2022.tif")



for(i in 1:length(meta1)) {
  cat("Proccess", i, "\n")
  
  tryCatch({
    r <- terra::rast(meta1[i])
    r2 <- terra::resample(r, ex2)
    May_Jul[[i]] <- r2
  }, error = function(e) {
    cat("No exist", meta1[i], ":", conditionMessage(e), "\n")
    Jan_Apr[[i]] <- NULL
  })
}


Jan_Apr2 <- Jan_Apr[!sapply(Jan_Apr, is.null)]

#fl2=sprc(May_Jul2)
#fl3=mosaic(fl2,fun='max')
#et3=max(fl3,na.rm = T)

# Planejar execução paralela com sessões separadas
plan(multisession, workers = 27)

stacked <- terra::rast(Jan_Apr2)
et2 <- terra::app(stacked, fun = max, na.rm = TRUE) #Take less time to mosaic

writeRaster(et2, "ECOSTRESS_EVAP_Jan_April_2022.tif")
plot(et2)


#May to July -------------------------------------------------------------------
May_Jul <- list()

#for(i in 1:length(meta2)) {
#  cat(i,'\n')
#  r <-terra::rast(meta2[i])
#  r2=terra::resample(r,ex2)
#  May_Jul[[i]] <-r2
#}

for(i in 1:length(meta2)) {
  cat("Proccess", i, "\n")
  
  tryCatch({
    r <- terra::rast(meta2[i])
    r2 <- terra::resample(r, ex2)
    May_Jul[[i]] <- r2
  }, error = function(e) {
    cat("No exist", meta2[i], ":", conditionMessage(e), "\n")
    May_Jul[[i]] <- NULL
  })
}


May_Jul2 <- May_Jul[!sapply(May_Jul, is.null)]

#fl2=sprc(May_Jul2)
#fl3=mosaic(fl2,fun='max')
#et3=max(fl3,na.rm = T)

# Planejar execução paralela com sessões separadas
plan(multisession, workers = 27)

stacked <- terra::rast(May_Jul2)
et3 <- terra::app(stacked, fun = max, na.rm = TRUE) #Take less time to mosaic

writeRaster(et3, "ECOSTRESS_EVAP_May_July_2022.tif")
plot(et3)

#August to September -----------------------------------------------------------
Aug_Sep <- list()

plan(multisession, workers = 27)

for(i in 1:length(meta3)) {
  cat("Proccess", i, "\n")
  
  tryCatch({
    r <- terra::rast(meta3[i])
    r2 <- terra::resample(r, ex2)
    Aug_Sep[[i]] <- r2
  }, error = function(e) {
    cat("No exist", meta3[i], ":", conditionMessage(e), "\n")
    Aug_Sep[[i]] <- NULL
  })
}


Aug_Sep2 <- Aug_Sep[!sapply(Aug_Sep, is.null)]
#for(i in 1:length(meta3)) {
#  cat(i,'\n')
#  r <-terra::rast(meta3[i])
#  r2=terra::resample(r,ex2)
#  Aug_Sep[[i]] <-r2
#}


#fl2=sprc(Aug_Sep)
#fl3=mosaic(fl2,fun='max')
#et4=max(fl3,na.rm = T)
stacked <- terra::rast(Aug_Sep2)
et4 <- terra::app(stacked, fun = max, na.rm = TRUE) #Take less time to mosaic

writeRaster(et4, "ECOSTRESS_EVAP_Aug_Sep_2022.tif")
plot(et4)

#October to December -----------------------------------------------------------
Oct_Dec <- list()

plan(multisession, workers = 27)

for(i in 1:length(meta4)) {
  cat("Proccess", i, "\n")
  
  tryCatch({
    r <- terra::rast(meta4[i])
    r2 <- terra::resample(r, ex2)
    Oct_Dec[[i]] <- r2
  }, error = function(e) {
    cat("No exist", meta4[i], ":", conditionMessage(e), "\n")
    Oct_Dec[[i]] <- NULL
  })
}


#for(i in 1:length(meta4)) {
#  cat(i,'\n')
#  r <-terra::rast(meta4[i])
#  r2=terra::resample(r,ex2)
#  Oct_Dec[[i]] <-r2
#}


#fl2=sprc(Oct_Dec)
#fl3=mosaic(fl2,fun='max')
#et5=max(fl3,na.rm = T)

Oct_Dec2 <- Oct_Dec[!sapply(Oct_Dec, is.null)]

stacked <- terra::rast(Oct_Dec2)
et5 <- terra::app(stacked, fun = max, na.rm = TRUE) #Take less time to mosaic

writeRaster(et5, "ECOSTRESS_EVAP_Oct_Dec_2022.tif")
plot(et5)



# ------------------------------------------------------------------------------

library(tidyverse)
library(terra)
library(sf)
library(future) #Multicore work

parallel::detectCores()

#Load Data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/ECOSTRESS_day")
dir()

wet1 = rast("ECOSTRESS_EVAP_Jan_April_2022.tif")
wet2 = rast("ECOSTRESS_EVAP_Oct_Dec_2022.tif")

dry1 = rast("ECOSTRESS_EVAP_May_July_2022.tif")
dry2 = rast("ECOSTRESS_EVAP_Aug_Sep_2022.tif")

#Joing wet and dry seasons -----------------------------------------------------
plan(multisession, workers = 27)

#Wet
stacked <- c(wet1, wet2)
wet <- terra::app(stacked, fun = mean, na.rm = TRUE)

writeRaster(wet, "ECOSTRESS_EVAP_WetSeason_2022.tif")
plot(wet)

#Dry
stacked <- c(dry1, dry2)
dry <- terra::app(stacked, fun = mean, na.rm = TRUE)

writeRaster(dry, "ECOSTRESS_EVAP_DrySeason_2022.tif")
plot(dry)

#Annual
#wet = rast("ECOSTRESS_EVAP_WetSeason_2022.tif")
#dry = rast("ECOSTRESS_EVAP_DrySeason_2022.tif")
stacked <- c(wet, dry)
annual <- terra::app(stacked, fun = mean, na.rm = TRUE)

writeRaster(annual, "ECOSTRESS_EVAP_annual_2022.tif")
plot(annual)

















#xx=do.call('rbind',results)
#max(xx$nrow) # 4275
#max(xx$ncol) # 4718

#et_all<- rast(results)
#et2=max(et_all,na.rm = T)
#et2= app(et_all, function(i) max(i,na.rm=T))

et_all = terra::app(c(wet, dry), fun = mean, na.rm = TRUE)
plot(et_all)

##
sf=rast("G:/Meu Drive/Postdoc_UFRA/Papers/Serra (Elias et al)/Analises_Elias/Rasters/MB_Forest_age_30m.tif")
plot(sf)
names(sf)="age"
etx=terra::resample(et_all,sf)
plot(etx)

etx2=c(sf,etx)
res=zonal(etx, sf, "mean", na.rm=TRUE)
res$mm_m2=(res$mean*0.0864)*0.408
res$Season = "All"

#1 W m-2 = 0.0864 MJ m-2 day-1
#1 MJ m-2 day-1 = 0.408 mm day-1


# grafico de resultados (Fully year)
ggplot(res,aes(age,mm_m2))+
  geom_point()+
  stat_smooth(span=0.5)


#Dry Season
etx=terra::resample(dry,sf)
plot(etx)

etx2=c(sf,etx)
dry_res=zonal(etx, sf, "mean", na.rm=TRUE)
dry_res$mm_m2=(dry_res$mean*0.0864)*0.408
dry_res$Season = "Dry"

ggplot(dry_res,aes(age,mm_m2))+
  geom_point()+
  stat_smooth(span=0.5)+
  labs(title = "Dry Season (May - September")


#Wet Season
etx=terra::resample(wet,sf)
plot(etx)

etx2=c(sf,etx)
wet_res=zonal(etx, sf, "mean", na.rm=TRUE)
wet_res$mm_m2=(wet_res$mean*0.0864)*0.408
wet_res$Season = "Wet"

ggplot(wet_res,aes(age,mm_m2))+
  geom_point()+
  stat_smooth(span=0.5)+
  labs(title = "Wet Season (October - April")



df = rbind(res[c(1,3,4)], dry_res[c(1,3,4)], wet_res[c(1,3,4)])
colnames(df) = c("Age", "mm", "Season")

ggplot(df, aes(Age, mm, col = Season))+
  geom_point()+
  stat_smooth(span=0.5)


write.csv(df, "ECOSTRESS_Vegetation_Age.csv", row.names = F)

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


