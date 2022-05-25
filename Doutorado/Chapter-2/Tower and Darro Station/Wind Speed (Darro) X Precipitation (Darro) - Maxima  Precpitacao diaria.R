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

rawppt3 = ggarrange(rawppt, rawppt2, ncol = 2)

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

maxppt3 = ggarrange(maxppt, maxppt2, ncol = 2); maxppt3

ggsave(filename = "WS-Prec_darro_all.png", plot = maxppt3,
             path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 30, height = 13, units = "cm", dpi = 300)

#ggsave(filename = "WS-Prec_darro.png", plot = maxppt,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 17, height = 15, units = "cm", dpi = 300)

#ggsave(filename = "WS-Prec_darro_facet.png", plot = maxppt2,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 23, height = 15, units = "cm", dpi = 300)

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
ggplot(chi, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chi")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "green")+
  ylim(0, 1)+
  theme_bw()

ggplot(chibar, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chibar")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "green")+
  ylim(-1, 1)+
  theme_bw()


#Unite Tail dependence results from Darro and ERA5 ------------------------------------
chi$data = c("Darro Station")
chibar$data = c("Darro Station")


chi_era = read.csv("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Dados ERA5/CHI_ERA5.csv", sep = ",")
chibar_era = read.csv("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Dados ERA5/CHIBAR_ERA5.csv", sep = ",")

chi_torre = read.csv("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Dados Torre/CHI_Tower.csv", sep = ",")
chibar_torre = read.csv("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Dados Torre/CHIBAR_Tower.csv", sep = ",")



chi2 = rbind(chi, chi_era, chi_torre)
chibar2 = rbind(chibar, chibar_era, chibar_torre)
colnames(chi2)[5] = c("Dataset")
colnames(chibar2)[5] = c("Dataset")

chi_plot = ggplot(chi2, aes(quant, value, fill = Dataset, linetype = Dataset))+
  geom_line()+
  labs(x = NULL, y = "Chi", title = "A")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.25)+
  scale_fill_manual(values =  c("#33a02c", "red", "#1f78b4"))+
  theme_bw()+
  theme(legend.position = c(30,30)); chi_plot

chibar_plot = ggplot(chibar2, aes(quant, value, fill = Dataset, linetype = Dataset))+
  geom_line()+
  labs(x = "Quantile theshold q", y = "Chibar", title = "B")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.25)+
  scale_fill_manual(values =  c("#33a02c", "red", "#1f78b4"))+
  theme_bw()+
  theme(legend.position = c(.8,.2)); chibar_plot


chis = ggarrange(chi_plot, chibar_plot, ncol = 1); chis


#ggsave(filename = "WS-Prec_darro_era_chi_chibar.png", plot = chis,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 15, height = 20, units = "cm", dpi = 300)


#ggsave(filename = "WS-Prec_darro_chi.png", plot = chi_plot,
#       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 20, height = 10, units = "cm", dpi = 300)

#ggsave(filename = "WS-Prec_darro_chibar.png", plot = chibar_plot,
#       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 20, height = 10, units = "cm", dpi = 300)

































