
#https://stackoverflow.com/questions/68757629/sf-generate-random-points-with-maximal-distance-condition

# packages
library(tidyverse)
library(sf)
library(terra)
#> Linking to GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
path='G:\\.shortcut-targets-by-id\\1pYKOdIkbspykcgyKHEESBxfCvAKnn-EP\\Modelos Nacentes 2023\\Modelos_nascentes\\'
#path='G:\\Meu Drive\\Modelos Nacentes 2023\\Modelos_nascentes\\'
setwd(paste(path,'nascentes_corrego_fundo',sep = ''))
dir()

n_obs=st_read('Nascentes_corrego_fundo_filtradas.shp')
head(n_obs)


n_fbds=st_read("Nascentes_corrego_fundo_FBDS.shp" )
head(n_fbds)

#############
st_crs(n_obs)$proj4string
st_crs(n_fbds)$proj4string

n_obs2 = n_obs%>%
  mutate(long = unlist(map(n_obs$geometry,1)),
         lat = unlist(map(n_obs$geometry,2)),
         nasc=paste('nasc',1:44,sep='_'))%>%
  st_transform(n_obs,
  crs = st_crs(n_fbds))

st_crs(n_obs2) == st_crs(n_fbds)


# matriz de distancias
df <- st_distance(n_obs2, n_fbds)

# create colnames and rownames to make work easier
#colnames(df) <- df.laea$CD_ESTACAO
rownames(df) <- n_obs2$nasc

df2=units::drop_units(as.data.frame(df))%>%
  mutate(nasc=row.names(df))

57-44 # nascentes 
1-(44/57)

#funcao para lidar com na
# https://stackoverflow.com/questions/74359531/get-the-rowwise-minimum-of-certain-columns-excluding-0-and-na
safe <- function(x, f, ...) ifelse(all(is.na(x)), NA, 
                                   ifelse(all(is.na(x) | x == 0), 
                                          0, f(x[x > 0], na.rm = TRUE, ...)))

df3=df2%>%
  rowwise()%>% 
  mutate(mindist = safe(c_across(V1:V57), min))%>%
  ungroup()%>%
  dplyr::select(nasc,mindist)


hist(df3$mindist)
ggplot(df3, aes(x = mindist)) +
  geom_histogram(color='black',bins = 9)+
  theme_minimal()+
  labs(x='Distância entre nascentes observadas e preditas (m)')


df3%>%filter(mindist>500)
10/44
###


## extrair a elevação para cada um dos pontos
library(elevatr)
# link abaixo mostra exemplo para extrair ponto do minimo - talvez funciona
# https://stackoverflow.com/questions/70660882/r-get-column-names-for-changed-rows
#View(n_obs2)
df_elev <- data.frame(
                nasc = n_obs2$nasc,
                x = n_obs2$long,
                y = n_obs2$lat)



df4=n_elev%>%
  full_join(df3,by=c('nasc'='nasc'))
write.csv(df4,'nascentes_elevacao.csv')

ggplot(df4, aes(x=elevation,y=mindist)) +
  geom_point()+
  theme_minimal()+
  stat_smooth(method = 'lm')+
  labs(y="Distância entre nascente observada e predita (m)",
       x="Altitude (m)")

m0=lm(mindist~elevation,df4)
summary(m0)


####
df5=df4%>%
  mutate(dist_c=cut(mindist,breaks = seq(0,1800,300),
                    labels= seq(300,1800,300)-150))%>%
  group_by(dist_c)%>%
  summarise(sd_alt=mean(elevation))%>%
  ungroup()%>%
  mutate(dist_c=as.numeric(as.character(dist_c)))

ggplot(df5, aes(x=sd_alt,y=dist_c)) +
  geom_point()+
  theme_minimal()+
 # lims(y=c(-600,1800))+
  stat_smooth(method = 'lm')+
  labs(y="Classes de distâncias das diferenças",
       x="Altitude média (m)")

m0=lm(dist_c~sd_alt,df5)
summary(m0)

# ideia geral sobre como pegar os dados de id de nascentes mais proxima
# https://stackoverflow.com/questions/70660882/r-get-column-names-for-changed-rows


old <- data.frame(var1 = c(1, 2, 3, 5), var2 = c(1.2, 1.7, 3.1, 5))

dfx=df2%>%
  mutate(pred_min = apply(.[,1:57], 1, function(x) names(x)[which.min(x)]))%>%
  select(nasc,pred_min)%>%
  mutate(id=paste('pred',substr(pred_min,2,4),sep='_'))
 

n_fbds2 = n_fbds%>%
  mutate(long = unlist(map(n_fbds$geometry,1)),
         lat = unlist(map(n_fbds$geometry,2)),
         id=paste('pred',1:57,sep='_'))
pred_elev <- data.frame(
  x = n_fbds2$long,
  y = n_fbds2$lat,
  id=n_fbds2$id)

n_elev <- get_elev_point(pred_elev, prj = st_crs(n_fbds2)$proj4string,
                         z = 14, src = "aws")

n_elevf=dfx%>%
  full_join(n_elev,by=c('id'='id'))%>%
  filter(is.na(nasc)==F)
write.csv(n_elevf,'elevation_pred.csv')
  
### testar inclinação

#nproj="+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
nproj="+proj=aea +lat_0=-10 +lon_0=-20 +lat_1=-10 +lat_2=-20 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

n_obs3 <-st_transform(n_obs, nproj)


loc_df <- data.frame(x = runif(6,min=sf::st_bbox(n_obs)$xmin, 
                               max=sf::st_bbox(n_obs)$xmax),
                     y = runif(6,min=sf::st_bbox(n_obs)$ymin, 
                               max=sf::st_bbox(n_obs)$ymax))

srtm <- get_elev_raster(n_obs3,z=14,expand = 5000)
srtm2=as(srtm, "SpatRaster")
srtm3=terra::project(x=srtm2,y=nproj)
plot(srtm3)

slope <- terrain(srtm3, v=c("slope","TPI","TRI"))





#https://www.webmapit.com.br/inpe/topodata/
pred=terra::extract(slope,n_obs3,fun=mean,na.rm=TRUE)

write.csv(pred,'slope_nascentes_corfundo.csv')
plot(n_obs3)
plot(srtm,add=T)













library(elevatr)
library(sf)
data(lake)
lake_buff  <- st_buffer(lake, 1000)
loc_df <- data.frame(x = runif(6,min=sf::st_bbox(lake)$xmin, 
                               max=sf::st_bbox(lake)$xmax),
                     y = runif(6,min=sf::st_bbox(lake)$ymin, 
                               max=sf::st_bbox(lake)$ymax))

x <- get_elev_raster(locations = loc_df, prj = st_crs(lake) , z=10)
x <- get_elev_raster(lake, z = 12)
x <- get_elev_raster(lake, src = "gl3", expand = 5000)
x <- get_elev_raster(lake_buff, z = 10, clip = "locations")

