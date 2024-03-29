#---------------------------------------------------------------------------------------
#Wind Speed (Tower) X Precipitation (Darro) - Block Maxima Approach and Tail Dependence
#---------------------------------------------------------------------------------------
#Eduardo Q Marques 16-05-2022
#eduardobio2009@gmail.com
#---------------------------------------------------------------------------------------

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
torre = read.csv("Dados_Vento_Torre_Controle.csv", sep = ",")

#Select variables and filter data to rainy season to 2010-2020 -----------------
df = darro[,c(16, 13)]

colnames(df) = c("date", "ppt")

#df$date = as.Date(df$date)

df$date2 = as.numeric(substr(df$date, 1, 4))
df$month = as.character(substr(df$date, 6, 7))

df = df %>% filter(date2 %in% c(2014,2015,2016,2017,2018,2019,2020)) #Consistent data time series
df = df %>% filter(month %in% c("10","11","12","01","02","03","04")) #Rainy months to AW climate
df = df %>% filter(ppt <100) #Outlier maybe a error in registration

#Tower -------------------------------------------------------------------------- 
#torre$datetime = substr(torre$datetime, 1, 10)
torre = torre[,c(2,4)]

#torre$datetime = as.Date(torre$datetime)
colnames(torre) = c("date", "ws")

torre$date2 = as.numeric(substr(torre$date, 1, 4))
torre$month = as.character(substr(torre$date, 6, 7))

torre = torre %>% filter(date2 %in% c(2014,2015,2016,2017,2018,2019,2020)) #Consistent data time series
torre = torre %>% filter(month %in% c("10","11","12","01","02","03","04")) #Rainy months to AW climate

#Join data ---------------------------------------------------------------------
df2 = full_join(torre, df, id = "date")
df2$Date = as.factor(df2$date2)

#Plot data
eqm = c('#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928')

rawppt = ggplot(df2, aes(x=ppt, y=ws))+
  geom_point(aes(col = Date), alpha = 0.7, size = 3)+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Precipitation (mm)", y = "Max Wind Speed (m/s)", title = "B")+
  scale_color_manual(values = eqm)+
  theme_bw()+
  theme(legend.position = c(30, 30))#; rawppt

rawppt2 = ggplot(df2, aes(x=ppt, y=ws))+
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

#ggsave(filename = "WS(Tower)-Prec(darro)_RAW.png", plot = rawppt,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 13, height = 13, units = "cm", dpi = 300)

#Block Maxima diary precipitation -----------------------------------------------
df3 = df2 %>% na.omit()
df3$date = substr(df3$date, 1, 10)
df3 <- blockmaxxer(df3, blocks = df3$date, which="ppt") #Function only blocking precipitation

#Plot data
maxppt = ggplot(df3, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.7, size = 3, col = "#33a02c")+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Maximum Precipitation (mm)", y = "Max Wind Speed (m/s)", title = "C")+
  #scale_color_manual(values = eqm)+
  theme_bw()+
  theme(legend.position = c(30, 30))#; maxppt

maxppt2 = ggplot(df3, aes(x=ppt, y=ws))+
  geom_point(alpha = 0.7, col = "#33a02c", size = 2)+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Maximum Precipitation (mm)", y = NULL, title = "D")+
  facet_wrap(~Date)+
  #scale_color_manual(values = eqm)+
  theme_bw()+
  theme(legend.position = c(30, 30))#; maxppt2

maxppt3 = ggarrange(maxppt, maxppt2, ncol = 2); maxppt3

#ggsave(filename = "WS(Tower)-Prec(darro)__all.png", plot = maxppt3,
#             path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 30, height = 13, units = "cm", dpi = 300)

#ggsave(filename = "WS-Prec_darro.png", plot = maxppt,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 17, height = 15, units = "cm", dpi = 300)

#ggsave(filename = "WS-Prec_darro_facet.png", plot = maxppt2,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 23, height = 15, units = "cm", dpi = 300)


#Extract tail dependence values and Bootstrapping ------------------------------------
df2 = df3
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
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "red")+
  ylim(0, 1)+
  theme_bw(); chi_plot

chibar_plot = ggplot(chibar, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chibar")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "red")+
  ylim(-1, 1)+
  theme_bw(); chibar_plot

#Export data -----------------------------------------------------------------------------
chi$data = c("Control Tower")
chibar$data = c("Control Tower")

write.csv(chi, "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Dados Torre/CHI_Tower.csv", sep = ",", row.names = F)
write.csv(chibar, "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Dados Torre/CHIBAR_Tower.csv", sep = ",", row.names = F)



































#Block Maxima by 5 days windows ------------------------------------------------
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

#Comparison -----------------------------------------------------------------
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
       x = "Precipitation (max mm/5d)",
       y = "Wind Speed (m/s)")+
  theme_bw()

img = ggarrange(r1, r2, ncol = 2)#; img

r1 = ggplot(df2)+
  geom_density(aes(ws), fill = "red", alpha = 0.35)+
  labs(x = "Wind Speed (m/s)")+
  theme_bw()

r2 = ggplot(df3)+
  geom_density(aes(ws), fill = "red", alpha = 0.35)+
  labs(x = "Wind Speed (m/s)")+
  theme_bw()

img2 = ggarrange(r1, r2, ncol = 2)#; img2

r1 = ggplot(df2)+
  geom_density(aes(ppt), fill = "blue", alpha = 0.35)+
  labs(x = "Precipitation (max mm/d)")+
  theme_bw()

r2 = ggplot(df3)+
  geom_density(aes(ppt), fill = "blue", alpha = 0.35)+
  labs(x = "Precipitation (max mm/5d)")+
  theme_bw()

img3 = ggarrange(r1, r2, ncol = 2)#; img3

img4 = ggarrange(img, img2, img3, ncol = 1); img4
#ggsave(filename = "WS-Prec_darro.png", plot = img4,
#       path = "C:/Users/Eduardo Q Marques/Desktop", width = 35, height = 30, units = "cm", dpi = 300)



#Extract tail dependence values from Block Maxima Approach data --------------------
df3 = df3 %>% na.omit()

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
  theme_bw(); chi_plot

chibar_plot = ggplot(chibar, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chibar")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "green")+
  theme_bw(); chibar_plot


#ggsave(filename = "WS-Prec_darro_chi.png", plot = chi_plot,
 #      path = "C:/Users/Eduardo Q Marques/Desktop", width = 20, height = 10, units = "cm", dpi = 300)


#ggsave(filename = "WS-Prec_darro_chibar.png", plot = chibar_plot,
 #      path = "C:/Users/Eduardo Q Marques/Desktop", width = 20, height = 10, units = "cm", dpi = 300)
