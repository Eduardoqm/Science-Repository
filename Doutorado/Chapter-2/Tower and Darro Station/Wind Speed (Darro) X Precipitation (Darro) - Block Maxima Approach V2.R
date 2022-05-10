#---------------------------------------------------------------------------------------
#Wind Speed (Darro) X Precipitation (Darro) - Block Maxima Approach and Tail Dependence
#---------------------------------------------------------------------------------------
#Eduardo Q Marques 09-05-2022
#eduardobio2009@gmail.com
#---------------------------------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(viridis)
library(fmsb)
library(lubridate)

#Darro data ====================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Dados das torres")

darro = read.csv("Master_Estacao_Darro_2020.csv", sep = ",")

#Select variables and filter data to rainy season to 2010-2020 -----------------
df = darro %>% 
  select(Date, windSpd, ppt)
colnames(df) = c("date", "ws", "ppt")

df$date = as.Date(df$date)

df$date2 = as.numeric(substr(df$date, 1, 4))
df$month = as.character(substr(df$date, 6, 7))

df = df %>% filter(date2 %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)) #Consistent data time series
df = df %>% filter(month %in% c("10","11","12","01","02","03","04")) #Rainy months to AW climate
df = df %>% filter(ppt <100) #Outlier maybe a error in registration


#Block Maxima by 5 days windows ------------------------------------------------
#Filter by diary maximun
#df2 = df %>% 
  #na.omit() %>% 
  #group_by(date) %>% 
  #summarise(ws = max(ws), ppt = max(ppt))

#The Maximun filter can be substitute by blockmaxxer (doing exact the same)
library(extRemes)
df2 <- blockmaxxer(df, blocks = df$date, which = "ppt")
df2b <- blockmaxxer(df, blocks = df$date, which = "ws")
df2$ws = df2b$ws

#Block Maxima Approach (5 days)
df3 = df2 #df3 will receive the modifications

#Functions to moving 5 days
ppt5d = function(z) {
  w = z-2
  k = z+2
  max(df2$ppt[w:k])
}

ws5d = function(z) {
  w = z-2
  k = z+2
  max(df2$ws[w:k])
}

#Loop to modify all dataframe
for (x in 3:length(df3$date)) {
  df3$ppt[x] = ppt5d(x)
  df3$ws[x] = ws5d(x)
}

df3 = df3[c(-1,-2, -2211, -2212),] #First and second are not maximum

ggplot(df3, aes(x=ppt, y=ws))+ #Verify second result
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs(title = "Block Maxima Approach (5 days)",
       x = "Precipitation (max mm/5d)",
       y = "Wind Speed (m/s)")+
  theme_bw()


#Comparison -----------------------------------------------------------------
r0 = ggplot(df, aes(x=ppt, y=ws))+ #Verify raw data result
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs(title = "Raw data 2010-2020",
       x = "Precipitation (mm)",
       y = "Wind Speed (m/s)")+
  theme_bw()

r1 = ggplot(df2, aes(x=ppt, y=ws))+ #Verify first result
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs(title = "Maximum per day",
       x = "Precipitation (max mm/d)",
       y = "Wind Speed (m/s)")+
  theme_bw()

r2 = ggplot(df3, aes(x=ppt, y=ws))+ #Verify second result
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs(title = "Block Maxima Approach (5 days)",
       x = "Precipitation (max mm/5d)",
       y = "Wind Speed (m/s)")+
  theme_bw()

img = ggarrange(r0, r1, r2, ncol = 3)


r0 = ggplot(df)+
  geom_density(aes(ws), fill = "red", alpha = 0.35)+
  labs(x = "Wind Speed (m/s)")+
  theme_bw()

r1 = ggplot(df2)+
  geom_density(aes(ws), fill = "red", alpha = 0.35)+
  labs(x = "Wind Speed (m/s)")+
  theme_bw()

r2 = ggplot(df3)+
  geom_density(aes(ws), fill = "red", alpha = 0.35)+
  labs(x = "Wind Speed (m/s)")+
  theme_bw()

img2 = ggarrange(r0, r1, r2, ncol = 3)


r0 = ggplot(df)+
  geom_density(aes(ppt), fill = "blue", alpha = 0.35)+
  labs(x = "Precipitation (mm)")+
  theme_bw()

r1 = ggplot(df2)+
  geom_density(aes(ppt), fill = "blue", alpha = 0.35)+
  labs(x = "Precipitation (max mm/d)")+
  theme_bw()

r2 = ggplot(df3)+
  geom_density(aes(ppt), fill = "blue", alpha = 0.35)+
  labs(x = "Precipitation (max mm/5d)")+
  theme_bw()

img3 = ggarrange(r0, r1, r2, ncol = 3)

#img4 = ggarrange(img, img2, img3, ncol = 1)
#ggsave(filename = "WS-Prec_darro.png", plot = img4,
#       path = "C:/Users/Eduardo Q Marques/Desktop", width = 35, height = 30, units = "cm", dpi = 300)



#Extract tail dependence values from Block Maxima Approach data --------------------
tq = c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1)

t1 = taildep(df3$ppt, df3$ws, 0.05)
taild = data.frame(tq[[1]], t1[[1]], t1[[2]])
colnames(taild) = c("quant", "chi", "chibar")

for (z in 2:20) {
  t = taildep(df3$ppt, df3$ws, (tq[[z]]))
  t2 = data.frame(tq[[z]], t[[1]], t[[2]])
  colnames(t2) = c("quant", "chi", "chibar")
  taild = rbind(taild, t2)
}

