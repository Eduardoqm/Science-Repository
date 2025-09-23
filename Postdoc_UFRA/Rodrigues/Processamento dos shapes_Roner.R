#Processamento dos Shapes Roner TCC

# Eduardo Q Marques 29-01-2025

library(terra)
library(sf)
library(lwgeom)
library(tidyverse)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Geodata/Vectors")
dir()

#Land Use
uso = sf::st_read("FBDS_uso_nordeste_PA2.shp")

uso$CLASSE_USO[uso$CLASSE_USO == "Ã¡rea antropizada"] = c("Antropizada")
uso$CLASSE_USO[uso$CLASSE_USO == "formaÃ§Ã£o florestal"] = c("Preservada")
uso$CLASSE_USO[uso$CLASSE_USO == "formaÃ§Ã£o nÃ£o florestal"] = c("Preservada")
uso$CLASSE_USO[uso$CLASSE_USO == "área antropizada"] = c("Antropizada")
uso$CLASSE_USO[uso$CLASSE_USO == "formação florestal"] = c("Preservada")
uso$CLASSE_USO[uso$CLASSE_USO == "formação não florestal"] = c("Preservada")

unique(uso$CLASSE_USO)

#APP Land Use
app_uso = st_read("APP_USO_MESCLADO.shp")

app_uso$CLASSE_USO[app_uso$CLASSE_USO == "Ã¡rea antropizada"] = c("APP antropizada")
app_uso$CLASSE_USO[app_uso$CLASSE_USO == "formaÃ§Ã£o florestal"] = c("APP preservada")
app_uso$CLASSE_USO[app_uso$CLASSE_USO == "formaÃ§Ã£o nÃ£o florestal"] = c("APP preservada")
app_uso$CLASSE_USO[app_uso$CLASSE_USO == "Ã¡rea edificada"  ] = c("APP antropizada")
app_uso$CLASSE_USO[app_uso$CLASSE_USO == "silvicultura"] = c("APP antropizada")
app_uso$CLASSE_USO[app_uso$CLASSE_USO == "Ã¡gua"] = c("APP preservada")

unique(app_uso$CLASSE_USO)

#Perennial Cultures
cult = st_read("culturas_permanentes_completo.shp")

cult$CLASSE_USO[cult$CLASSE_USO == "citros"] = c("Citros")
cult$CLASSE_USO[cult$CLASSE_USO == "dendê"] = c("Dende")
cult$CLASSE_USO[cult$CLASSE_USO == "pimenta"] = c("Pimenta")

unique(cult$CLASSE_USO)

#CAR
car = st_read("CAR_nordeste_Para.shp")

#Setting coordinate systems ----------------------------------------------------
uso = st_transform(uso, "+proj=longlat +datum=WGS84 +no_defs")
app_uso = st_transform(app_uso, "+proj=longlat +datum=WGS84 +no_defs")
cult = st_transform(cult, "+proj=longlat +datum=WGS84 +no_defs")
car = st_transform(car, "+proj=longlat +datum=WGS84 +no_defs")

#Intersecting Polygons ---------------------------------------------------------
sf_use_s2(FALSE) #Enable “sf” default to using it whenever a spatial operation is done.


car_uso = st_intersection(car[,c(1,11)], uso[,c(2,3)])
car_app = st_intersection(car[,c(1,11)], app_uso[,c(5,9)])
car_cult = st_intersection(car[,c(1,11)], st_make_valid(cult[,c(2,4)]))


all = rbind(car_uso, car_app, car_cult)


all2=st_make_valid(all)%>%
  mutate(prob=st_is_valid(all))%>%
  dplyr::mutate(area_ha=units::drop_units(st_area(.)* 0.0001))

unique(all2$CLASSE_USO)

#Saving Process ----------------------------------------------------------------
#Shape
all3 <- st_collection_extract(all2, "POLYGON")
st_write(all3, "Roner.shp")


#df = as.data.frame(all2[,c(1,2,5)])
#write.csv(df, "Dados_Roner.csv")





