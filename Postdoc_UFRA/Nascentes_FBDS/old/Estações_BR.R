
#https://stackoverflow.com/questions/68757629/sf-generate-random-points-with-maximal-distance-condition

# packages
library(tidyverse)
library(sf)
#> Linking to GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
path="G:\\Meu Drive\\UFRA\\Pesquisa\\Projetos\\Projetos_pesquisa\\INMET"

setwd("G:\\Meu Drive\\UFRA\\Pesquisa\\Projetos\\Projetos_pesquisa\\INMET")
dir()

d=read.csv("Catalogoestacoesautomaticas.csv")
head(d)

#d2=d%>%
#  mutate(
#    lat=as.numeric(gsub(",",".",VL_LATITUDE)),
#    lon=as.numeric(gsub(",",".",VL_LONGITUDE))
#  )
# create data
df <- st_as_sf(d,coords = c("VL_LONGITUDE","VL_LATITUDE"),remove = F, crs="EPSG:4326")
plot(df)
df.laea = st_transform(
  df,
  crs = "+proj=laea +x_0=4600000 +y_0=4600000 +lon_0=0.13 +lat_0=0.24 +datum=WGS84 +units=m"
)


plot(df.laea)
sampled_points <- st_sample(
  x = st_as_sfc(st_bbox(df.laea)),
  type = "SSI",
  r = 100000#, # threshold distance (in metres)
  #n = 1000 # number of points
)
sf::st_crs(sampled_points)<-"+proj=laea +x_0=4600000 +y_0=4600000 +lon_0=0.13 +lat_0=0.24 +datum=WGS84 +units=m"
sampled_points$id=paste("p",1:dim(sampled_points)[1],sep = "")
plot(sampled_points)
# Check result
par(mar = rep(0, 4))
plot(st_as_sfc(st_bbox(df.laea)), reset = FALSE)
plot(sampled_points, add = TRUE, pch = 16)
plot(df.laea, add = TRUE, pch = 1)



#############
df <- st_distance(df.laea, df.laea)

# create colnames and rownames to make work easier
colnames(df) <- df.laea$CD_ESTACAO
rownames(df) <- df.laea$SG_ESTADO

df2=units::drop_units(as.data.frame(df))
#names(df2)=paste(names(df2),1:568,sep = "_")

df3=df2%>%
  mutate(across(everything(), na_if, 0))%>%
  gather(cod,dist_m,A422:A255)%>%
  group_by(cod)%>%
  slice_min(dist_m,n=4)%>%
  full_join(df.laea,by=c('cod'='CD_ESTACAO'))%>%
  group_by(SG_ESTADO)%>%
  summarise(dist_km=mean(dist_m,na.rm = T)/1000)
head(df3)


###
ggplot(df3, aes(x=reorder(SG_ESTADO,-dist_km), y=dist_km)) +
  geom_bar(stat="identity")+
  theme_minimal()+
  coord_flip()+
  labs(x=NULL,y="Distância média das 4 estações mais próximas (km)")


#df3=df2%>%
#  mutate(across(everything(), na_if, 0))%>%
#  summarise(across(A422:A255, min,na.rm=T))%>%
#  gather(cod,dist_m,A422:A255)%>%
#  full_join(df.laea,by=c('cod'='CD_ESTACAO'))%>%
#  group_by(SG_ESTADO)%>%
#  summarise(dist_km=mean(dist_m,na.rm = T)/1000)
#head(df3)



# matrix of county to county distances
distances <- st_distance(df.laea, sampled_points)

# create colnames and rownames to make work easier
colnames(distances) <- sampled_points$id
rownames(distances) <- df.laea$CD_ESTACAO

# distance from county Mecklenburg / 100 values, inc. zeroes
df4=units::drop_units(as.data.frame(distances))

df5=df4%>%
  gather(idd,dist_m,p1:p649)%>%
  filter(dist_m>1000)%>%
  group_by(idd)%>%
  summarise(dist_min=min(dist_m,na.rm = T)/1000)%>%
  filter(dist_min>80)%>%
  select(idd)%>%
  unique()%>%
  mutate(new_ws="yes")

  

df6=sampled_points%>%
  full_join(df5,by=c('id'='idd'))%>%
  filter(new_ws=='yes')

# Check result
par(mar = rep(0, 4))
plot(st_as_sfc(st_bbox(df.laea)), reset = FALSE)
plot(df.laea, add = TRUE, pch = 16,col="gray")
plot(df6, add = TRUE, pch = 1)

setwd(paste(path,'\\shapes',sep = ""))
dir()

br=st_read("BR_UF_2021.shp")
plot(br, add = TRUE, pch = 1)

df6_ll=st_transform(df6, crs=sf::st_crs(br))
df7=st_intersection(df6_ll,br)

par(mar = rep(0, 4))
plot(st_as_sfc(st_bbox(br)), reset = FALSE)
plot(br, add = TRUE, pch = 1)
plot(df7, add = TRUE, pch = 16,col='black')


st_write(df7, "New_station_final.shp")


write.csv(df3,'Sumario_dist_estados.csv')
