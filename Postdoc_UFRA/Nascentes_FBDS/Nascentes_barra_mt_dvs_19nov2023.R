library(sf)
library(elevatr)
library(tidyverse)
pt='G:\\.shortcut-targets-by-id\\1pYKOdIkbspykcgyKHEESBxfCvAKnn-EP\\Modelos Nacentes 2023\\Modelos_nascentes\\validacao'
setwd(pt)
dir()
n_fbds=st_read("nascentes_FBDS_all.shp" )
head(n_fbds)

n_fbds2=n_fbds%>%
  mutate(long = unlist(map(n_fbds$geometry,1)),
         lat = unlist(map(n_fbds$geometry,2)))
         
pred_elev <- data.frame(
  x = n_fbds2$long,
  y = n_fbds2$lat,
  id=n_fbds2$id)

n_elev <- elevatr::get_elev_point(pred_elev, 
                        prj = st_crs(n_fbds2)$proj4string,
                         z = 14, src = "aws")

n_elevf=n_fbds2%>%st_drop_geometry()%>%
  full_join(n_elev,by=c('id'='id'))


### importar dados - nascentes observadas
dir()
n_obs=st_read("nascentes_observadas_all.shp")
head(n_obs)

n_obs2=n_obs%>%
  mutate(long = unlist(map(n_obs$geometry,1)),
         lat = unlist(map(n_obs$geometry,2)))

pred_obs <- data.frame(
  x = n_obs2$long,
  y = n_obs2$lat,
  id_obs=n_obs$id_obs)

ele_obs <- elevatr::get_elev_point(pred_obs, 
                        prj = st_crs(n_obs2)$proj4string,
                        z = 14, src = "aws")


ele_obsf=n_obs2%>%st_drop_geometry()%>%
    full_join(ele_obs,by=c('id_obs'='id_obs'))%>%
    select(id_obs,tipo_n,categ,elevation)
  
names(ele_obsf)=c("id_obs","tipo_n",'categ_obs',"elev_obs")

### distancia
# matriz de distancias
df <- st_distance(n_obs2, n_fbds2)

 n_obs3=n_obs2%>%st_drop_geometry()%>%
      select(id_obs, comp_fbds)

dist_m=list()
for(i in 1:dim(n_obs3)[1]){
  cat(i,'\n')
  dist_m[i]=ifelse(n_obs3$comp_fbds[i]==0,NA,
                   df[i,n_obs3$comp_fbds[i]])
}

n_obs3$dist_m=dist_m 

#### juntar as informações

dff=n_obs3%>%
  full_join(ele_obsf,by=c('id_obs'='id_obs'))%>%
  full_join(n_elevf,by=c('comp_fbds'='id'))%>%
  mutate(
    elevat_m=ifelse(is.na(elev_obs),elevation,elev_obs),
    dif_elevat=elev_obs-elevation,
    categ=ifelse(is.na(categ),categ_obs,categ))

### plot
dff2=dff%>%filter(categ%in%c('acerto','omissao','comissao'))

ggplot(dff2,aes(categ,elevat_m))+
  geom_boxplot()

