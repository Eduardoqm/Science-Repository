
# script para analisar ET do Ecosstress para 
# florestas secundárias da Amazônia

library(tidyverse)
library(terra)
library(sf)
path="G:\\Meu Drive\\UFRA\\Pesquisa\\colaboracoes\\Fernando Elias\\Artigo_clima_seconfor_2025\\ET\\"

setwd(paste(path,'guama',sep = ""))
dir()
guama=read_sf("Microrregiao_Guama.shp")
guama2=st_transform(guama,crs = 4326)

setwd(paste(path,'Toy',sep = ""))
dir()

fl=dir(pattern = "ECO3*")

ex=rast('ECO3ETPTJPL.001_EVAPOTRANSPIRATION_PT_JPL_ETcanopy_doy2022198192703_aid0009.tif')
ex2 <- rast(ext(guama2), resolution=res(ex))
res(ex)
results <- list()

for(i in 1:length(fl)) {
  cat(i,'\n')
  r <-terra::rast(fl[i])
  r2=terra::resample(r,ex2)
  results[[i]] <-r2
}

fl2=sprc(results)
fl3=mosaic(fl2,fun='max')
et2=max(fl3,na.rm = T)
plot(et2)
#xx=do.call('rbind',results)
#max(xx$nrow) # 4275
#max(xx$ncol) # 4718

#et_all<- rast(results)
#et2=max(et_all,na.rm = T)
#et2= app(et_all, function(i) max(i,na.rm=T))

dir()

##
sf=rast("Toy_MB_Forest_age.tif")
plot(sf)
names(sf)="age"
etx=terra::resample(et2,sf)
plot(etx)

etx2=c(sf,etx)
res=zonal(etx, sf, "mean", na.rm=TRUE)
res$mm_m2=(res$max*0.0864)*0.408

#1 W m-2 = 0.0864 MJ m-2 day-1
#1 MJ m-2 day-1 = 0.408 mm day-1




# grafico de resultados - individuos
ggplot(res,aes(age,mm_m2))+
  geom_point()+
  stat_smooth(span=0.5)
##

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


