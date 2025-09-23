#Analysis Roner TCC

# Eduardo Q Marques 05-02-2025

library(tidyverse)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Eduardo/Documents/Programing/Science-Repository/Postdoc_UFRA/Rodrigues")
dir()

df = read.csv2("Dados_Roner.csv", sep = ",")
head(df)

df$area_ha = as.numeric(df$area_ha)
df$Preservada = as.numeric(df$Preservada)
df$Antropizada = as.numeric(df$Antropizada)
df$APP.antropizada = as.numeric(df$APP.antropizada)
df$APP.preservada = as.numeric(df$APP.preservada)
df$Dende = as.numeric(df$Dende)
df$Pimenta = as.numeric(df$Pimenta)
df$Citros = as.numeric(df$Citros)

df$app_ant = as.numeric(df$app_ant)
df$preser = as.numeric(df$preser)
df$past = as.numeric(df$past)
df$agri = as.numeric(df$agri)

#Basic information -------------------------------------------------------------
sum(na.omit(df$area_ha)) #Area of CARs
sum(na.omit(df$Preservada))
sum(na.omit(df$Antropizada))

df$Perm = (df$Dende + df$Citros + df$Pimenta)
sum(na.omit(df$Perm))

df$AppTotal = (df$APP.antropizada + df$APP.preservada)
sum(na.omit(df$AppTotal))

#Analysis ----------------------------------------------------------------------
df2 = df %>% 
  mutate(app_ant = ifelse(is.na(app_ant) == T, 0, app_ant),
         area_ha = ifelse(is.na(area_ha) == T, 0, area_ha)) %>% 
  filter(area_ha > 0)


#Effect in APPS Antropization
app1 = ggplot(df2, aes(past*100,app_ant*100))+
  geom_point(col = "orange", size = 4, alpha = 0.1)+
  labs(x = "Pasture (%)", y = "Anthropized PPA (%)",
       title = "a)")+
  stat_smooth(col = "black", method = "lm")+
  theme_bw(); app1

past1=lm(app_ant~past,df2)
summary(past1)


app2 = ggplot(df2, aes(agri*100,app_ant*100))+
  geom_point(col = "purple", size = 4, alpha = 0.1)+
  labs(x = "Permanent agriculture (%)", y = "Anthropized PPA (%)",
       title = "b)")+
  stat_smooth(col = "black", method = "lm")+
  theme_bw(); app2

agri1=lm(app_ant~agri,df2)
summary(agri1)




#Effect in Intact Vegetation
reser1 = ggplot(df2, aes(past*100,preser*100))+
  geom_point(col = "orange", size = 4, alpha = 0.1)+
  labs(x = "Pastagem (%)", y = "Legal Reserve (%)",
       title = "a)")+
  stat_smooth(col = "black", method = "lm")+
  theme_bw(); reser1

past2=lm(preser~past,df2)
summary(past2)


reser2 = ggplot(df2, aes(agri*100,preser*100))+
  geom_point(col = "purple", size = 4, alpha = 0.1)+
  labs(x = "Permanent agriculture (%)", y = "Reserva Legal (%)",
       title = "b)")+
  stat_smooth(col = "black", method = "lm")+
  theme_bw(); reser2

agri2=lm(preser~agri,df2)
summary(agri2)



#Export graphics ---------------------------------------------------------------
ggsave(filename = "APP_Antrop_past.png", plot = app1,
       width = 20, height = 12, units = "cm", dpi = 300)


ggsave(filename = "APP_Antrop_perm.png", plot = app2,
       width = 20, height = 12, units = "cm", dpi = 300)


ggsave(filename = "Reserva_past.png", plot = reser1,
       width = 20, height = 12, units = "cm", dpi = 300)


ggsave(filename = "Reserva_perm.png", plot = reser2,
       width = 20, height = 12, units = "cm", dpi = 300)





