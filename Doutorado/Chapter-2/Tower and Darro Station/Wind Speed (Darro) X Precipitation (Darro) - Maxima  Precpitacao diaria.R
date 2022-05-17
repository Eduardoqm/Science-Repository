#-------------------------------------------------------------------------
#Wind Speed (Darro) X Precipitation (Darro) - Maxima diary precipitation
#-------------------------------------------------------------------------
#Eduardo Q Marques 17-05-2022
#eduardobio2009@gmail.com
#-------------------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(viridis)
library(fmsb)
library(lubridate)
library(extRemes)
library(boot)

#Darro data ====================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Dados das torres")

darro = read.csv("Master_Estacao_Darro_2020.csv", sep = ",")


#Filter data ------------------------------------------------------------------
df = darro %>% 
  select(Date, windSpd, ppt)
colnames(df) = c("date", "ws", "ppt")

df$date = as.Date(df$date)
df$date2 = as.numeric(substr(df$date, 1, 4))
df$month = as.character(substr(df$date, 6, 7))

#Filters
df = df %>% filter(date2 %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)) #Consistent data time series
df = df %>% filter(month %in% c("10","11","12","01","02","03","04")) #Rainy months to AW climate
df = df %>% filter(ppt <100) #Outlier maybe a error in registration

#Plot data
ggplot(df, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.5, size = 3, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Maximum Precipitation (max mm/day)", y = "Wind Speed (m/s)", title = "Raw data 2010 - 2020")+
  theme_bw()

ggplot(df, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  facet_wrap(~date2)+
  theme_bw()


#Block Maxima diary precipitation -----------------------------------------------
df2 <- blockmaxxer(df, blocks = df$date, which="ppt") #Function only blocking precipitation

#Plot data
ggplot(df2, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.5, size = 3, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Maximum Precipitation (mm)", y = "Wind Speed (m/s)", title = "Block by max Pretipition per day and co-current Wind Speed")+
  theme_bw()

ggplot(df2, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  facet_wrap(~date2)+
  theme_bw()


#Extract tail dependence values and Bootstrapping ------------------------------------
tq = seq(.05, 1, .01); length(tq)

t1 = taildep(df2$ppt, df2$ws, 0.05)
taild = data.frame(tq[[1]], t1[[1]], t1[[2]])
colnames(taild) = c("quant", "chi", "chibar")

#Chi ---------------------------------------------------------------------------------
#Function to extract Chi data from tail dependence
chifun = function(formula, data, indices) {
  df2 <- data[indices,] # selecting sample with boot 
  fit <- taildep(df2$ppt, df2$ws, 0.05)
  return(fit[[1]])
} 

f1 =c(df2$ppt, df2$ws, 0.05)
chifun(formula = f1, data = df2) #Just a test

#Performing 1000 replications with boot 
output <- boot(data=df2, statistic=chifun, 
               R=1000, formula=f1)

#Obtaining a confidence interval of 95%
inter = boot.ci(output, type="perc")

chi = data.frame(tq[[1]], inter$t0, inter$percent[1,4], inter$percent[1,5])
colnames(chi) = c("quant", "value", "low", "upp")

#Loop to do all Chi quantiles
for (z in 2:96) {
  print(tq[[z]])
  chifun = function(formula, data, indices) {
    df2 <- data[indices,]
    fit <- taildep(df2$ppt, df2$ws, (tq[[z]]))
    return(fit[[1]])
  }
  
  f1 =c(df2$ppt, df2$ws, (tq[[z]]))
  
  output <- boot(data=df2, statistic=chifun, 
                 R=1000, formula=f1)
  inter = boot.ci(output, type="perc")
  t2 = data.frame(tq[[z]], inter$t0, inter$percent[1,4], inter$percent[1,5])
  colnames(t2) = c("quant", "value", "low", "upp")
  chi = rbind(chi, t2)
}


#Chibar ---------------------------------------------------------------------------------
chibarfun = function(formula, data, indices) {
  df2 <- data[indices,] # selecting sample with boot 
  fit <- taildep(df2$ppt, df2$ws, 0.05)
  return(fit[[2]])
} 

f1 =c(df2$ppt, df2$ws, 0.05)
chibarfun(formula = f1, data = df2) #Just a test

#Performing 1000 replications with boot 
output <- boot(data=df2, statistic=chibarfun, 
               R=1000, formula=f1)

#Obtaining a confidence interval of 95%
inter = boot.ci(output, type="perc")

chibar = data.frame(tq[[1]], inter$t0, inter$percent[1,4], inter$percent[1,5])
colnames(chibar) = c("quant", "value", "low", "upp")

#Loop to do all Chibar quantiles
for (z in 2:96) {
  print(tq[[z]])
  chibarfun = function(formula, data, indices) {
    df2 <- data[indices,]
    fit <- taildep(df2$ppt, df2$ws, (tq[[z]]))
    return(fit[[2]])
  }
  
  f1 =c(df2$ppt, df2$ws, (tq[[z]]))
  
  output <- boot(data=df2, statistic=chibarfun, 
                 R=1000, formula=f1)
  inter = boot.ci(output, type="perc")
  t2 = data.frame(tq[[z]], inter$t0, inter$percent[1,4], inter$percent[1,5])
  colnames(t2) = c("quant", "value", "low", "upp")
  chibar = rbind(chibar, t2)
}

#Plot results ------------------------------------------------------------------------------
chi_plot = ggplot(chi, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chi")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "green")+
  theme_bw(); chi_plot

chibar_plot = ggplot(chibar, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chibar")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "green")+
  theme_bw(); chibar_plot




























#Chi ---------------------------------------------------------------------------------
#Function to extract Chi data from tail dependence
chifun = function(formula, data, indices) {
  df <- data[indices,] # selecting sample with boot 
  fit <- taildep(df$ppt, df$ws, 0.05)
  return(fit[[1]])
} 

f1 =c(df$ppt, df$ws, 0.05)
chifun(formula = f1, data = df) #Just a test

#Performing 1000 replications with boot 
output <- boot(data=df, statistic=chifun, 
               R=1000, formula=f1)

#Obtaining a confidence interval of 95%
inter = boot.ci(output, type="perc")

chi = data.frame(tq[[1]], inter$t0, inter$percent[1,4], inter$percent[1,5])
colnames(chi) = c("quant", "value", "low", "upp")

#Loop to do all Chi quantiles
for (z in 2:96) {
  print(tq[[z]])
  chifun = function(formula, data, indices) {
    df <- data[indices,]
    fit <- taildep(df$ppt, df$ws, (tq[[z]]))
    return(fit[[1]])
  }
  
  f1 =c(df$ppt, df$ws, (tq[[z]]))
  
  output <- boot(data=df, statistic=chifun, 
                 R=1000, formula=f1)
  inter = boot.ci(output, type="perc")
  t2 = data.frame(tq[[z]], inter$t0, inter$percent[1,4], inter$percent[1,5])
  colnames(t2) = c("quant", "value", "low", "upp")
  chi = rbind(chi, t2)
}


#Chibar ---------------------------------------------------------------------------------
chibarfun = function(formula, data, indices) {
  df <- data[indices,] # selecting sample with boot 
  fit <- taildep(df$ppt, df$ws, 0.05)
  return(fit[[2]])
} 

f1 =c(df$ppt, df$ws, 0.05)
chibarfun(formula = f1, data = df) #Just a test

#Performing 1000 replications with boot 
output <- boot(data=df, statistic=chibarfun, 
               R=1000, formula=f1)

#Obtaining a confidence interval of 95%
inter = boot.ci(output, type="perc")

chibar = data.frame(tq[[1]], inter$t0, inter$percent[1,4], inter$percent[1,5])
colnames(chibar) = c("quant", "value", "low", "upp")

#Loop to do all Chibar quantiles
for (z in 2:96) {
  print(tq[[z]])
  chibarfun = function(formula, data, indices) {
    df <- data[indices,]
    fit <- taildep(df$ppt, df$ws, (tq[[z]]))
    return(fit[[2]])
  }
  
  f1 =c(df$ppt, df$ws, (tq[[z]]))
  
  output <- boot(data=df, statistic=chibarfun, 
                 R=1000, formula=f1)
  inter = boot.ci(output, type="perc")
  t2 = data.frame(tq[[z]], inter$t0, inter$percent[1,4], inter$percent[1,5])
  colnames(t2) = c("quant", "value", "low", "upp")
  chibar = rbind(chibar, t2)
}

#Plot results ------------------------------------------------------------------------------
chi_plot = ggplot(chi, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chi")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "green")+
  theme_bw(); chi_plot

chibar_plot = ggplot(chibar, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chibar")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "green")+
  theme_bw(); chibar_plot














































































img = ggplot(df, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.5, size = 3, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  #stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), col = "red", aplha = 0.3)+
  #geom_abline(data = df3, aes(intercept = Intercept, slope = Precipitation), col = "red", aplha = 0.3)+
  labs( x = "Maximum Precipitation (max mm/day)", y = "Wind Speed (m/s)",
        title = "Only Maximum Precipitation per day")+
  scale_color_viridis()+
  theme_bw()

img2 = ggplot(df)+
  geom_density(aes(ws), fill = "red", alpha = 0.35)+
  labs(x = "Wind Speed (m/s)")+
  theme_bw()

img3 = ggplot(df)+
  geom_density(aes(ppt), fill = "blue", alpha = 0.35)+
  labs(x = "Precipitation (max mm/d)")+
  theme_bw()


img4 = ggarrange(img, img2, img3, ncol = 1); img4

ggplot(df, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  #stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), show.legend = TRUE, col = "red", aplha = 0.3)+
  labs( x = "Daily accumulated precipitation", y = "Wind Speed per day")+
  facet_wrap(~date2)+
  scale_color_viridis()+
  theme_bw()



#ggsave(filename = "WS-Prec_darro.png", plot = img4,
#              path = "C:/Users/Eduardo Q Marques/Desktop", width = 12, height = 30, units = "cm", dpi = 300)



tq = seq(.05, 1, .01)

for (z in 1:96) {
  w = taildep(df2$ppt, df2$ws, (tq[[z]]))
  print(w)
}





















