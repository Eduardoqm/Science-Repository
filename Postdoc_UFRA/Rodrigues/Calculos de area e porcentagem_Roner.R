
#Calculations os area and fractions

#Eduardo Q Marques & Divino Siverio 03-02-2025

library(terra)
library(sf)
library(lwgeom)
library(tidyverse)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Geodata/Vectors")
dir()

#CAR
car = st_read("CAR_nordeste_Para.shp")
head(car)

car2=st_make_valid(car)%>%
  dplyr::mutate(area_ha=units::drop_units(st_area(.)* 0.0001))%>%
  st_drop_geometry()%>%
  select(COD_IMOVEL,area_ha)

head(car2)
# dados uso
d=st_read("Roner.shp")
head(d)

d2=d%>%
  st_drop_geometry()%>%
  group_by(COD_IMOVEL,CLASSE_USO)%>%
  summarise(
    area_uso_ha=sum(area_ha))%>%
  pivot_wider(names_from = CLASSE_USO, values_from = area_uso_ha,values_fill = 0)%>%
  left_join(car2,by=c('COD_IMOVEL'='COD_IMOVEL'))

head(d2)

d3=d2%>%
  mutate(app_ant=`APP antropizada`/(`APP antropizada`+`APP preservada`),
         agri=(Dende+Citros+Pimenta)/area_ha,
         antrop=Antropizada/area_ha,
         past=(Antropizada-(Dende+Citros+Pimenta))/area_ha)%>%
  mutate(past=ifelse(past<0|past>1,NA,past),
         preser=Preservada/area_ha,
         agri=ifelse(agri>1,NA,agri))
  
head(d3)

setwd("G:/Meu Drive/Postdoc_UFRA/Coorientacoes/Roner_Rodrigues")
write.csv(d3, "Dados_Roner.csv", row.names = F)


ggplot(d3,aes(agri,app_ant))+
  geom_point()+
  stat_smooth(method = 'lm')

ggplot(d3,aes(past,app_ant))+
  geom_point()+
  stat_smooth()

ggplot(d3,aes(agri,preser))+
  geom_point()+
  stat_smooth(method = 'lm')

ggplot(d3,aes(past,preser))+
  geom_point()+
  stat_smooth(method = 'lm')

## modelo estatistico
agri1=lm(app_ant~agri+I(agri^2),d3)
summary(agri1)

past1=lm(app_ant~past+I(past^2),d3)
summary(past1)


agri2=lm(preser~agri+I(agri^2),d3)
summary(agri2)


past2=lm(preser~past+I(past^2),d3)
summary(past2)













