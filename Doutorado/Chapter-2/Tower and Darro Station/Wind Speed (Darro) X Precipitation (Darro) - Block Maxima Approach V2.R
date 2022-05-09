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

r0 = ggplot(df, aes(x=ppt, y=ws))+ #Verify first result
  geom_point()+geom_smooth(method = "lm", col = "royalblue")+
  stat_cor(show.legend = F)+labs(title = "Raw data 2010-2020"); r0


#Block Maxima by 5 days windows ------------------------------------------------
#Filter by diary maximun
df2 = df %>% 
  na.omit() %>% 
  group_by(date) %>% 
  summarise(ws = max(ws), ppt = max(ppt))

r1 = ggplot(df2, aes(x=ppt, y=ws))+ #Verify first result
  geom_point()+geom_smooth(method = "lm", col = "royalblue")+
  stat_cor(show.legend = F)+labs(title = "Maximum per day"); r1

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

r2 = ggplot(df3, aes(x=ppt, y=ws))+ #Verify second result
  geom_point()+geom_smooth(method = "lm", col = "royalblue")+
  stat_cor(show.legend = F)+labs(title = "Block Maxima Approach (5 days)"); r2




ggplot()+
  geom_point(data = df, aes(date, ppt), size = 1)+
  geom_point(data = df2, aes(date, ppt), col = "red", alpha = 0.35, size = 1.5)+
  geom_point(data = df3, aes(date, ppt), col = "blue", alpha = 0.35, size = 2)















library(extRemes)

cor(df3$ppt, df3$ws)

plot(df3$ppt, df3$ws)

taildep(df3$ppt, df3$ws, 0.95)

taildep.test(df$ppt, df$ws) # Recall that the null hypothesis is tail dependence!


df <- blockmaxxer(df, blocks = df$date, which = "ppt")


cor(df$ppt, df$ws)

plot(df$ppt, df$ws)

taildep(df$ppt, df$ws, 0.95)

taildep.test(df$ppt, df$ws) # Recall that the null hypothesis is tail dependence!



df$ws2 = log(df$ws)
df$ppt2 = log(df$ppt)



ggplot(df3, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.5, size = 3, col = "royalblue")+
  geom_smooth(method = "gam", col = "black")+
  stat_cor(show.legend = F)+
  #stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), col = "red", aplha = 0.3)+
  #geom_abline(data = df3, aes(intercept = Intercept, slope = Precipitation), col = "red", aplha = 0.3)+
  labs( x = "Maximum Precipitation (mm/day)", y = "Maximun Wind Speed (m/s) - day",
        title = "Precipitation vs Wind Speed (Darro Station)")+
  scale_color_viridis()+
  theme_bw()

df3$date2 = as.numeric(substr(df3$date, 1, 4))
ggplot(df3, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "gam", col = "black")+
  stat_cor(show.legend = F)+
  #stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), show.legend = TRUE, col = "red", aplha = 0.3)+
  labs( x = "Daily accumulated precipitation", y = "Wind Speed per day")+
  facet_wrap(~date2)+
  scale_color_viridis()+
  theme_bw()

ggplot(df, aes(x=ppt2, y=ws2, col = date2))+
  geom_point(alpha = 0.7, size = 2)+
  geom_smooth(method = "gam", col = "royalblue")+
  stat_cor(show.legend = F)+
  #stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), col = "red", aplha = 0.3)+
  #geom_abline(data = df3, aes(intercept = Intercept, slope = Precipitation), col = "red", aplha = 0.3)+
  labs( x = "Daily accumulated precipitation", y = "Wind Speed per day",
        title = "Precipitation (Darro Station) vs Wind Speed (Tower)")+
  scale_color_viridis()+
  theme_bw()

ggplot(df, aes(x=ppt2, y=ws2))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "gam", col = "black")+
  stat_cor(show.legend = F)+
  #stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), show.legend = TRUE, col = "red", aplha = 0.3)+
  labs( x = "Daily accumulated precipitation", y = "Wind Speed per day")+
  facet_wrap(~date2)+
  scale_color_viridis()+
  theme_bw()








