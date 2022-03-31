#-------------------------------
# First Code in RGEE
#
# Eduardo Q Marques 30-03-2022
#-------------------------------

library(rgee)
library(reticulate)

Sys.which("python")   # system default
Sys.which("python3") 
use_python(Sys.which("python3")) 

#ee_clean_pyenv() # Remove reticulate system variables
#ee_Initialize()

#rgee::ee_install want to store the environment variables: EARTHENGINE_PYTHON 
#and EARTHENGINE_ENV in your .Renviron file to use the Python path:
#  /home/equeiroz/.virtualenvs/rgee/bin/python in future sessions.

#Call the GEE user
#ee_Initialize() ---------------------------------------------------------------
ee_Initialize(user = 'eduardobio2009@gmail.com') # Use the argument email is not mandatory, but it's helpful to change of EE user.

#Select data from GEE ----------------------------------------------------------
dataset <- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_EVI')$filterDate('2017-01-01', '2017-12-31')
ee_print(dataset)

#Select satellite image --------------------------------------------------------
landsat <- dataset$select('EVI')
class(landsat)

ee_print(landsat)

#Select band -------------------------------------------------------------------
evi <- landsat$select('EVI')$toBands()  
class(evi)

ee_print(evi)

#Make a vector of area ---------------------------------------------------------
region <- ee$Geometry$BBox(-76.7, 42.2, -76.2, 42.7)
Map$centerObject(region, 11)

colorizedVis <- list(
  min=0.0,
  max=1.0,
  palette=c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)

evi02jul <- evi$select("20170704_EVI")
Map$addLayer(evi02jul, colorizedVis, 'Landsat 8 EVI 02-July-2017')
