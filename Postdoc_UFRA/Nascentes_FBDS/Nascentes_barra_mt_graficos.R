#Regressions between elevations and slope

#Created by: Divino V. Silverio

#Adapted by: Eduardo Q Marques  01-05-2024

library(tidyverse)

path="G:/Meu Drive/Postdoc_UFRA/Papers/Parados/Nascentes_FBDS (Silverio et al)/Modelos_nascentes"
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


