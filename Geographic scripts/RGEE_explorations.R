
library(rgee)

ee_Initialize()

createTimeBand <-function(img) {
  year <- ee$Date(img$get('system:time_start'))$get('year')$subtract(1991L)
  ee$Image(year)$byte()$addBands(img)
}

collection <- ee$
  ImageCollection('NOAA/DMSP-OLS/NIGHTTIME_LIGHTS')$
  select('stable_lights')$
  map(createTimeBand)

col_reduce <- collection$reduce(ee$Reducer$linearFit())
col_reduce <- col_reduce$addBands(
  col_reduce$select('scale'))
ee_print(col_reduce)

Map$setCenter(9.08203, 47.39835, 3)
Map$addLayer(
  eeObject = col_reduce,
  visParams = list(
    bands = c("scale", "offset", "scale"),
    min = 0,
    max = c(0.18, 20, -0.18)
  ),
  name = "stable lights trend"
)

#Extract precipitation values
library(tidyverse)
library(rgee)
library(sf)
library(geojson)

# ee_reattach() # reattach ee as a reserve word
ee_Initialize()

#Shape
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

#Terraclimate data from 2001 by monthly
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2000-01-01", "2001-01-01")$
  map(function(x) x$reproject("EPSG:4326")$select("pr"))

ee_nc_rain <- ee_extract(x = terraclimate, y = nc, fun = ee$Reducer$max(), sf = FALSE)
colnames(ee_nc_rain) <- sprintf("%02d", 1:12)
ee_nc_rain$name <- nc$NAME

#Plot precipitation
ee_nc_rain %>%
  pivot_longer(-name, names_to = "month", values_to = "pr") %>%
  ggplot(aes(x = month, y = pr, group = name, color = pr)) +
  geom_line(alpha = 0.4) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal()



























