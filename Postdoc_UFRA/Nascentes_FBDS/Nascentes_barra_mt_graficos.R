#Regressions between elevations and slope

#Created by: Divino V. Silverio

#Adapted by: Eduardo Q Marques  01-05-2024

library(tidyverse)

path="G:/My Drive/Postdoc_UFRA/Papers/Nascentes_FBDS (Silverio et al)/Modelos_nascentes"
setwd(path)
dir()

df=readxl::read_excel("Nascentes_riacho_fundo_barra.xlsx")
head(df)
names(df)
dfx=df%>%filter(nasc!='nasc_31')


m0=lm(obs_elev_m~dif_elev,dfx)
summary(m0)


m1=lm(dif_elev~slope,dfx)
summary(m1)

p1=ggplot(dfx,aes(obs_elev_m,dif_elev))+
  geom_point(size = 3, alpha = 0.7, col = "royalblue")+
  stat_smooth(method = 'lm', col = "black")+
  labs(x='Elevation of observed water springs (m)',
       y='Observed and Precited elevation difference (m)',
       title = "b)")+
  annotate("text", x = c(522, 600), y = c(27, 25),
           label= c(expression("R" ^ 2), "= 0.15, p < 0.05"),
           color = "black", hjust = "inward")+
  theme_minimal();p1

ggsave(
  "Elevation.png",
  plot=p1,
  path = "G:/My Drive/Postdoc_UFRA/Papers/Nascentes_FBDS (Silverio et al)", 
  width = 12,
  height = 10,
  units='cm',
  dpi=300)




p2=ggplot(dfx,aes(slope,dif_elev))+
  geom_point(size = 3, alpha = 0.7, col = "royalblue")+
  stat_smooth(method = 'lm', col = "black")+
  labs(y='Observed and Precited elevation difference (m)',
       x='Slope', title = "a)")+
  annotate("text", x = c(41, 60), y = c(27, 25),
           label= c(expression("R" ^ 2), "= 0.22, p < 0.05"),
           color = "black", hjust = "inward")+
  theme_minimal(); p2

ggsave(
  "Slope.png",
  plot=p2,
  path = "G:/My Drive/Postdoc_UFRA/Papers/Nascentes_FBDS (Silverio et al)", 
  width = 12,
  height = 10,
  units='cm',
  dpi=300)



## fatores que explicam a diferença na distancia entre predito e observado
m1=lm(mindist_m~obs_elev_m+slope+TPI+TRI,dfx,na.action = 'na.fail')
ms_m1=MuMIn::dredge(m1);ms_m1
m1_f=lm(mindist_m~slope,df)
summary(m1_f)


## fatores que explicam a diferença na distancia entre predito e observado
m2=lm(dif_elev~obs_elev_m+slope*TRI+TPI,dfx,na.action = 'na.fail')
ms_m2=MuMIn::dredge(m2);ms_m2
m2_f=lm(dif_elev~slope,df)
summary(m2_f)




# histograma
ggplot(dfx, aes(x = mindist_m)) +
  geom_histogram(color='black',bins = 8)+
  theme_minimal()+
  labs(x='Distância entre nascentes observadas e preditas (m)')



dfx2=dfx%>%
  dplyr::select(mindist_m,dif_elev,obs_elev_m,slope,TPI,TRI)%>%
  gather(preditor,valor,obs_elev_m:TRI)


ggplot(dfx2, aes(x=mindist_m,y=valor)) +
  geom_point()+
  theme_minimal()+
  facet_wrap(preditor~.,scale = "free")+
  labs(x="Distância entre nascente observada e predita (m)",
       y=NULL)

  ggplot(dfx2, aes(x=dif_elev,y=valor)) +
    geom_point()+
    theme_minimal()+
    facet_wrap(preditor~.,scale = "free")+
    labs(x="Diferencça em altitude das nascentes preditas e observadas (m)",
         y=NULL)
  