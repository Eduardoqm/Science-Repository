#-------------------------------
# Exploration ERA5 on RGEE
#
# Eduardo Q Marques 30-03-2022
#-------------------------------

library(rgee)
library(reticulate)

#Actions that can be necessary -------------------------------------------------
#Sys.which("python3") 
#use_python(Sys.which("python3")) 
ee_install()
ee_check()
#ee_install_set_pyenv()

#Call the GEE user -------------------------------------------------------------
ee_Initialize(user = 'eduardobio2009@gmail.com')


#Make an buffer ----------------------------------------------------------------
pont = ee$Geometry$Point(-52.38159722705858,-13.076103567394911)

#Apply the buffer method to the Point
pointBuffer = pont$buffer(distance = 100000) #Distance in meters

#Select data from GEE ----------------------------------------------------------
dataset <- ee$ImageCollection('ECMWF/ERA5_LAND/HOURLY')$filterDate('2019-02-02', '2019-02-04')$filterBounds(pointBuffer)$map(function(image){image$clip(pointBuffer)})

v_wind <- dataset$select('v_component_of_wind_10m')
ee_print(v_wind)

v <- v_wind$select('v_component_of_wind_10m')$toBands() 
ee_print(v)


u_wind <- dataset$select('u_component_of_wind_10m')
ee_print(u_wind)

u <- u_wind$select('u_component_of_wind_10m')$toBands() 
ee_print(u)


#Mapping -----------------------------------------------------------------------
#Map$centerObject(region, 11)
Map$setCenter(-52.3848, -13.0796)

colorizedVis <- list(
  min=0.0,
  max=1.0,
  palette=c(
    "#000080","#0000D9","#4000FF","#8000FF","#0080FF","#00FFFF",
    "#00FF80","#80FF00","#DAFF00","#FFFF00","#FFF500","#FFDA00",
    "#FFB000","#FFA400","#FF4F00","#FF2500","#FF0A00","#FF00FF"
  )
)

v_wind_03022022 <- v$select("20190203T22_v_component_of_wind_10m")
u_wind_03022022 <- v$select("20190203T22_u_component_of_wind_10m")

Map$addLayer(v_wind_03022022, colorizedVis, 'v component of wind 10m')+
  Map$addLayer(pointBuffer)


#Download data as raster to Google Drive ---------------------------------------
vw = ee_as_raster(v) #this functions return errors... but this images gone to Google Drive (rgee_backup)
uw = ee_as_raster(u)






































