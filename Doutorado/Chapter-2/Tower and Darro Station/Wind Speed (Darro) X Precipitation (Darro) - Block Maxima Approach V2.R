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
df3 = df3[c(-2211, -2212),] #last two are not maximum
df3$idb = rep(seq(1,442), each = 5)

df3a <- blockmaxxer(df3, blocks = df3$idb, which = "ppt")
df3b <- blockmaxxer(df3, blocks = df3$idb, which = "ws")
df3a$ws = df3b$ws

#Functions to moving 5 days
#ppt5d = function(z) {
#  w = z-2
#  k = z+2
#  max(df2$ppt[w:k])
#}

#ws5d = function(z) {
#  w = z-2
#  k = z+2
#  max(df2$ws[w:k])
#}

#Loop to modify all dataframe
#for (x in 3:length(df3$date)) {
#  df3$ppt[x] = ppt5d(x)
#  df3$ws[x] = ws5d(x)
#}

#df3 = df3[c(-1,-2, -2211, -2212),] #First and second are not maximum

ggplot(df3a, aes(x=ppt, y=ws))+ #Verify second result
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_quantile(quantiles = c(.05,.1,.25,.5,.75,.90,.95), col = "red", aplha = 0.3)+
  stat_cor(show.legend = F)+
  labs(title = "Block Maxima Approach (5 days)",
       x = "Precipitation (max mm/5d)",
       y = "Wind Speed (m/s)")+
  theme_bw()

df3 = df3a

#Extract tail dependence values from Block Maxima Approach data --------------------
#tq = c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1)
tq = seq(.05, 1, .01)

t1 = taildep(df3$ppt, df3$ws, 0.05)
taild = data.frame(tq[[1]], t1[[1]], t1[[2]])
colnames(taild) = c("quant", "chi", "chibar")

#for (z in 2:20) {
for (z in 2:96) {
  t = taildep(df3$ppt, df3$ws, (tq[[z]]))
  t2 = data.frame(tq[[z]], t[[1]], t[[2]])
  colnames(t2) = c("quant", "chi", "chibar")
  taild = rbind(taild, t2)
}

ggplot(taild, aes(quant, chi))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold", y = "Chi")+
  theme_bw()

ggplot(taild, aes(quant, chibar))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold", y = "ChiBar")+
  theme_bw()

#Bootstraping --------------------------------------------------------------------
library(boot)

#Chi
chifun = function(formula, data, indices) {
  df <- data[indices,] # selecting sample with boot 
  fit <- taildep(df$ppt, df$ws, 0.05)
  return(fit[[1]])
} 

f1 =c(df$ppt, df$ws, 0.05)

chifun(formula = f1, data = df3)
# Performing 1500 replications with boot 
output <- boot(data=df3, statistic=chifun, 
               R=1000, formula=f1)

# Plotting the output
plot(output)

# Obtaining a confidence interval of 95%
inter = boot.ci(output, type="perc")

chi = data.frame(tq[[1]], inter$t0, inter$percent[1,4], inter$percent[1,5])
colnames(chi) = c("quant", "value", "low", "upp")

#Loop to do all Chi quantiles
#for (z in 2:20) {
for (z in 2:96) {
  chifun = function(formula, data, indices) {
    df <- data[indices,]
    fit <- taildep(df$ppt, df$ws, (tq[[z]]))
    return(fit[[1]])
  }
  
  f1 =c(df$ppt, df$ws, (tq[[z]]))
  
  output <- boot(data=df3, statistic=chifun, 
                 R=1000, formula=f1)
  inter = boot.ci(output, type="perc")
  t2 = data.frame(tq[[z]], inter$t0, inter$percent[1,4], inter$percent[1,5])
  colnames(t2) = c("quant", "value", "low", "upp")
  chi = rbind(chi, t2)
}


#Chibar
chibarfun = function(formula, data, indices) {
  df <- data[indices,] # selecting sample with boot 
  fit <- taildep(df$ppt, df$ws, 0.05)
  return(fit[[2]])
} 

f1 =c(df$ppt, df$ws, 0.05)

chibarfun(formula = f1, data = df3)
# Performing 1500 replications with boot 
output <- boot(data=df3, statistic=chibarfun, 
               R=1000, formula=f1)

# Plotting the output
plot(output)

# Obtaining a confidence interval of 95%
inter = boot.ci(output, type="perc")

chibar = data.frame(tq[[1]], inter$t0, inter$percent[1,4], inter$percent[1,5])
colnames(chibar) = c("quant", "value", "low", "upp")

#Loop to do all Chi quantiles
#for (z in 2:20) {
for (z in 2:96) {
  chibarfun = function(formula, data, indices) {
    df <- data[indices,]
    fit <- taildep(df$ppt, df$ws, (tq[[z]]))
    return(fit[[2]])
  }
  
  f1 =c(df$ppt, df$ws, (tq[[z]]))
  
  output <- boot(data=df3, statistic=chibarfun, 
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
  theme_bw()

chibar_plot = ggplot(chibar, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chibar")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "green")+
  theme_bw()


ggsave(filename = "WS-Prec_darro_chi.png", plot = chi_plot,
       path = "C:/Users/Eduardo Q Marques/Desktop", width = 20, height = 10, units = "cm", dpi = 300)


ggsave(filename = "WS-Prec_darro_chibar.png", plot = chibar_plot,
       path = "C:/Users/Eduardo Q Marques/Desktop", width = 20, height = 10, units = "cm", dpi = 300)




#########################################################################################################################################################
#All again with Max of Wind Speed and accumulated Rain ##################################################################################################
#########################################################################################################################################################

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
df2 = df %>% 
na.omit() %>% 
group_by(date) %>% 
summarise(ws = max(ws), ppt = sum(ppt))

#The Maximun filter can be substitute by blockmaxxer (doing exact the same)
#df2 <- blockmaxxer(df, blocks = df$date, which = "ppt")
#df2b <- blockmaxxer(df, blocks = df$date, which = "ws")
#df2$ws = df2b$ws

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
       x = "Precipitation (mm/d)",
       y = "Wind Speed (m/s)")+
  theme_bw()

r2 = ggplot(df3, aes(x=ppt, y=ws))+ #Verify second result
  geom_point(alpha = 0.7, size = 2, col = "royalblue")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs(title = "Block Maxima Approach (5 days)",
       x = "Precipitation (mm/d)",
       y = "Wind Speed (m/s)")+
  theme_bw()

img = ggarrange(r0, r1, r2, ncol = 3)#; img


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

img2 = ggarrange(r0, r1, r2, ncol = 3)#; img2


r0 = ggplot(df)+
  geom_density(aes(ppt), fill = "blue", alpha = 0.35)+
  labs(x = "Precipitation (mm)")+
  theme_bw()

r1 = ggplot(df2)+
  geom_density(aes(ppt), fill = "blue", alpha = 0.35)+
  labs(x = "Precipitation (mm/d)")+
  theme_bw()

r2 = ggplot(df3)+
  geom_density(aes(ppt), fill = "blue", alpha = 0.35)+
  labs(x = "Precipitation (mm/d)")+
  theme_bw()

img3 = ggarrange(r0, r1, r2, ncol = 3)#; img3

#img4 = ggarrange(img, img2, img3, ncol = 1)
#ggsave(filename = "WS-Prec_darro.png", plot = img4,
#       path = "C:/Users/Eduardo Q Marques/Desktop", width = 35, height = 30, units = "cm", dpi = 300)



#Extract tail dependence values from Block Maxima Approach data --------------------
#tq = c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1)
tq = seq(.05, 1, .01)

t1 = taildep(df3$ppt, df3$ws, 0.05)
taild = data.frame(tq[[1]], t1[[1]], t1[[2]])
colnames(taild) = c("quant", "chi", "chibar")

#for (z in 2:20) {
for (z in 2:96) {
  t = taildep(df3$ppt, df3$ws, (tq[[z]]))
  t2 = data.frame(tq[[z]], t[[1]], t[[2]])
  colnames(t2) = c("quant", "chi", "chibar")
  taild = rbind(taild, t2)
}

ggplot(taild, aes(quant, chi))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold", y = "Chi")+
  theme_bw()

ggplot(taild, aes(quant, chibar))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold", y = "ChiBar")+
  theme_bw()

#Bootstraping --------------------------------------------------------------------
library(boot)

#Chi
chifun = function(formula, data, indices) {
  df <- data[indices,] # selecting sample with boot 
  fit <- taildep(df$ppt, df$ws, 0.05)
  return(fit[[1]])
} 

f1 =c(df$ppt, df$ws, 0.05)

chifun(formula = f1, data = df3)
# Performing 1500 replications with boot 
output <- boot(data=df3, statistic=chifun, 
               R=1000, formula=f1)

# Plotting the output
plot(output)

# Obtaining a confidence interval of 95%
inter = boot.ci(output, type="perc")

chi = data.frame(tq[[1]], inter$t0, inter$percent[1,4], inter$percent[1,5])
colnames(chi) = c("quant", "value", "low", "upp")

#Loop to do all Chi quantiles
#for (z in 2:20) {
for (z in 2:96) {
  chifun = function(formula, data, indices) {
    df <- data[indices,]
    fit <- taildep(df$ppt, df$ws, (tq[[z]]))
    return(fit[[1]])
  }
  
  f1 =c(df$ppt, df$ws, (tq[[z]]))
  
  output <- boot(data=df3, statistic=chifun, 
                 R=1000, formula=f1)
  inter = boot.ci(output, type="perc")
  t2 = data.frame(tq[[z]], inter$t0, inter$percent[1,4], inter$percent[1,5])
  colnames(t2) = c("quant", "value", "low", "upp")
  chi = rbind(chi, t2)
}


#Chibar
chibarfun = function(formula, data, indices) {
  df <- data[indices,] # selecting sample with boot 
  fit <- taildep(df$ppt, df$ws, 0.05)
  return(fit[[2]])
} 

f1 =c(df$ppt, df$ws, 0.05)

chibarfun(formula = f1, data = df3)
# Performing 1500 replications with boot 
output <- boot(data=df3, statistic=chibarfun, 
               R=1000, formula=f1)

# Plotting the output
plot(output)

# Obtaining a confidence interval of 95%
inter = boot.ci(output, type="perc")

chibar = data.frame(tq[[1]], inter$t0, inter$percent[1,4], inter$percent[1,5])
colnames(chibar) = c("quant", "value", "low", "upp")

#Loop to do all Chi quantiles
#for (z in 2:20) {
for (z in 2:96) {
  chibarfun = function(formula, data, indices) {
    df <- data[indices,]
    fit <- taildep(df$ppt, df$ws, (tq[[z]]))
    return(fit[[2]])
  }
  
  f1 =c(df$ppt, df$ws, (tq[[z]]))
  
  output <- boot(data=df3, statistic=chibarfun, 
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
  theme_bw()

chibar_plot = ggplot(chibar, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chibar")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "green")+
  theme_bw()


ggsave(filename = "WS-Prec_darro_chi.png", plot = chi_plot,
       path = "C:/Users/Eduardo Q Marques/Desktop", width = 20, height = 10, units = "cm", dpi = 300)


ggsave(filename = "WS-Prec_darro_chibar.png", plot = chibar_plot,
       path = "C:/Users/Eduardo Q Marques/Desktop", width = 20, height = 10, units = "cm", dpi = 300)



