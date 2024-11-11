
#https://stackoverflow.com/questions/68757629/sf-generate-random-points-with-maximal-distance-condition

# packages
library(tidyverse)
library(sf)
#> Linking to GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
path="G:\\Meu Drive\\UFRA\\Pesquisa\\colaboracoes\\Leandro_Brasil\\Modelos_nascentes\\"

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
View(n_obs2)
df_elev <- data.frame(
                nasc = n_obs2$nasc,
                x = n_obs2$long,
                y = n_obs2$lat)

n_elev <- get_elev_point(df_elev, prj = st_crs(n_obs)$proj4string,
                         z = 14, src = "aws")

df4=n_elev%>%
  full_join(df3,by=c('nasc'='nasc'))

ggplot(df4, aes(x=mindist, y=elevation)) +
  geom_point()+
  theme_minimal()+
  stat_smooth(method = 'lm')+
  labs(x="Distância entre nascente observada e predita (m)",
       y="Altitude (m)")

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

ggplot(df5, aes(x=dist_c, y=sd_alt)) +
  geom_point()+
  theme_minimal()+
  stat_smooth(method = 'lm')+
  labs(x="Classes de distâncias das diferenças",
       y="Altitude média (m)")

m0=lm(dist_c~sd_alt,df5)
summary(m0)

# ideia geral sobre como pegar os dados de id de nascentes mais proxima
# https://stackoverflow.com/questions/70660882/r-get-column-names-for-changed-rows


old <- data.frame(var1 = c(1, 2, 3, 5), var2 = c(1.2, 1.7, 3.1, 5))

old%>%
  rowwise()%>% 
  mutate(mindist = safe(c_across(Var1:Var2), min))
