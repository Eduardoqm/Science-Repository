#-------------------------------------------------------------------------------
#Wind Speed (Darro) X Precipitation (Darro) - Maxima diary precipitation (Year)
#-------------------------------------------------------------------------------
#Eduardo Q Marques 23-05-2022
#eduardobio2009@gmail.com
#-------------------------------------------------------------------------------

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
df$Date = as.factor(df$date2)

#Filters
df = df %>% filter(date2 %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)) #Consistent data time series
df = df %>% filter(month %in% c("10","11","12","01","02","03","04")) #Rainy months to AW climate
df = df %>% filter(ppt <100) #Outlier maybe a error in registration

#Plot data
eqm = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928')

rawppt = ggplot(df, aes(x=ppt, y=ws))+
  geom_point(aes(col = Date), alpha = 0.7, size = 3)+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Precipitation (mm)", y = "Wind Speed (m/s)", title = "A")+
  scale_color_manual(values = eqm)+
  theme_bw()+
  theme(legend.position = c(30, 30))#; rawppt

rawppt2 = ggplot(df, aes(x=ppt, y=ws))+
  geom_point(aes(col = Date), alpha = 0.9, size = 2)+
  geom_smooth(method = "lm", col = "black")+
  labs( x = "Precipitation (mm)", y = NULL, title = "B")+
  stat_cor(show.legend = F)+
  facet_wrap(~date2)+
  scale_color_manual(values = eqm)+
  theme_bw()+
  theme(legend.position = c(30, 30))#; rawppt2

#rawppt3 = ggarrange(rawppt, rawppt2, ncol = 2)

#ggsave(filename = "WS-Prec_darro_RAW_all.png", plot = rawppt3,
#       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 35, height = 15, units = "cm", dpi = 300)

#ggsave(filename = "WS-Prec_darro_RAW.png", plot = rawppt,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 13, height = 13, units = "cm", dpi = 300)

#Block Maxima diary precipitation -----------------------------------------------
df2 <- blockmaxxer(df, blocks = df$date, which="ppt") #Function only blocking precipitation

#Plot data
maxppt = ggplot(df2, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.7, size = 3, col = '#ff7f00')+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Maximum Precipitation (mm)", y = "Wind Speed (m/s)", title = "A")+
  #scale_color_manual(values = eqm)+
  theme_bw()+
  theme(legend.position = c(30, 30)); maxppt

maxppt2 = ggplot(df2, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.7, size = 2, col = '#ff7f00')+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Maximum Precipitation (mm)", y = NULL, title = "B")+
  facet_wrap(~Date)+
  #scale_color_manual(values = eqm)+
  theme_bw()+
  theme(legend.position = c(30, 30)); maxppt2

#maxppt3 = ggarrange(maxppt, maxppt2, ncol = 2); maxppt3

#ggsave(filename = "WS-Prec_darro_all.png", plot = maxppt3,
#       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 30, height = 13, units = "cm", dpi = 300)

#ggsave(filename = "WS-Prec_darro.png", plot = maxppt,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 17, height = 15, units = "cm", dpi = 300)

#ggsave(filename = "WS-Prec_darro_facet.png", plot = maxppt2,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 23, height = 15, units = "cm", dpi = 300)




#Extract tail dependence values and Bootstrapping --------------------------------------

#Chi ===================================================================================
mstchi = function(df, yr){
  #Extract tail dependence values and Bootstrapping ------------------------------------
  df2 = df2 %>% filter(date2 == yr)
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
  for (z in 2:95) {
    print(chi)
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
  as.data.frame(chi)
}


chi = mstchi(df2, 2010); chi$Date = c(2010)

for (z in 2011:2020) {
  print(z)
  chix = mstchi(df2, z)
  chix$Date = z
  chi = rbind(chi, chix)
}


#ChiBar ====================================================================================
mstchibar = function(df, yr){
  #Extract tail dependence values and Bootstrapping ------------------------------------
  df2 = df2 %>% filter(date2 == yr)
  tq = seq(.05, 1, .01); length(tq)
  
  t1 = taildep(df2$ppt, df2$ws, 0.05)
  taild = data.frame(tq[[1]], t1[[1]], t1[[2]])
  colnames(taild) = c("quant", "chi", "chibar")
  
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
  for (z in 2:95) {
    print(chibar)
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
  as.data.frame(chibar)
}

chibar = mstchibar(df2, 2010); chibar$Date = c(2010)

for (z in 2011:2020) {
  print(z)
  chibarx = mstchibar(df2, z)
  chibarx$Date = z
  chibar = rbind(chibar, chibarx)
}

ggplot(chi, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chi")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "red")+
  facet_wrap(~Date)+
  ylim(0, 1)+
  theme_bw()

ggplot(chibar, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chibar")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "red")+
  facet_wrap(~Date)+
  ylim(-1, 1)+
  theme_bw()


#Export data -----------------------------------------------------------------------------
chi$data = c("Darro Station")
chibar$data = c("Darro Station")

write.csv(chi, "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Chi and Chibar/CHI_Darro.csv", sep = ",", row.names = F)
write.csv(chibar, "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Chi and Chibar/CHIBAR_Darro.csv", sep = ",", row.names = F)









