########################################################
# Landsat Time series for mean and relative difference #
# Confidence Intervals                                 #
#                                                      #
# Eduardo Q Marques 03-07-2023                         #
########################################################

library(tidyverse)
library(ggplot2)
library(mgcv)
library(boot)
#font_import()
#loadfonts(device = "win", quiet = TRUE)
#windowsFonts("Times New Roman" = windowsFont("Times New Roman"))

#Data ============================================================
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
df = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Modify Data =====================================================
df$year = substr(df$year, 1,4)
df$year = as.numeric(df$year)
df$index = as.character(df$index)
df$treat = as.character(df$treat)

#Modify elements of dataframe
df$index[df$index == "evi2"] <- c("EVI")
df$index[df$index == "ndvi"] <- c("NDVI")
df$index[df$index == "ndii"] <- c("NDII")
df$index[df$index == "grnd"] <- c("GRND")
df$index[df$index == "nbr"] <- c("NBR")
df$index[df$index == "nbr2"] <- c("NBR2")

df$treat[df$treat == "control"] <- c("Control")
df$treat[df$treat == "b3yr"] <- c("B3yr")
df$treat[df$treat == "b1yr"] <- c("B1yr")

#Resume repeat years with mean ====================================
df = df %>% 
  group_by(x, y, index, year, treat) %>% 
  summarise(value = mean(value))

summary(df$y)
min(df$y) #-13.08343 = 1000 meters
max(df$y) #-13.07422 = 0 meters

diffy = min(df$y) - max(df$y)
df$dist = ((max(df$y) - df$y)/diffy)*1000
df$dist = abs(df$dist)

summary(df$dist)


df$dist2 = c("a")
df$dist2[df$dist <= 250] = c("Edge")
df$dist2[df$dist > 250] = c("Interior")


#Mean time series =================================================
dfm = df %>% 
  group_by(year, treat, index) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()


# Confidence intervals ----------------------------------------------------
desconfidence = function(ind){
  grnd_df = df %>% 
    na.omit() %>% 
    filter(index == ind) %>% 
    filter(treat == "B1yr") %>% 
    filter(year == 1985)
  
  model = lm(value ~ 1, grnd_df)
  
  confint(model, level = 0.95)
  
  
  condef = data.frame(grnd_df$index[1], grnd_df$treat[1], grnd_df$year[1], mean(grnd_df$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2]) 
  colnames(condef) = c("index", "treat", "year", "mean", "low", "upper")
  
  
  grnd_df = df %>% 
    na.omit() %>% 
    filter(index == ind) %>% 
    filter(treat == "B1yr")
  
  ylp = c(1986:2011, 2013:2019)
  
  
  for (z in ylp) {
    data = grnd_df %>%
      filter(year == z)
    
    model = lm(value ~ 1, data)
    
    
    condefx = data.frame( grnd_df$index[1], grnd_df$treat[1], z, mean(data$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2])
    colnames(condefx) = c("index", "treat", "year", "mean", "low", "upper")
    
    condef = rbind(condef, condefx)
    
    print(confint(model, level = 0.95))
  }
  
  grnd_df = df %>% 
    na.omit() %>% 
    filter(index == ind) %>% 
    filter(treat == "B3yr") %>% 
    filter(year == 1985)
  
  model = lm(value ~ 1, grnd_df)
  
  confint(model, level = 0.95)
  
  
  condefB = data.frame(grnd_df$index[1], grnd_df$treat[1], grnd_df$year[1], mean(grnd_df$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2]) 
  colnames(condefB) = c("index", "treat", "year", "mean", "low", "upper")
  
  
  grnd_df = df %>% 
    na.omit() %>% 
    filter(index == ind) %>% 
    filter(treat == "B3yr")
  
  ylp = c(1986:2011, 2013:2019)
  
  
  for (z in ylp) {
    data = grnd_df %>%
      filter(year == z)
    
    model = lm(value ~ 1, data)
    
    
    condefxB = data.frame( grnd_df$index[1], grnd_df$treat[1], z, mean(data$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2])
    colnames(condefxB) = c("index", "treat", "year", "mean", "low", "upper")
    
    condefB = rbind(condefB, condefxB)
    
    print(confint(model, level = 0.95))
  }
  
  grnd_df = df %>% 
    na.omit() %>% 
    filter(index == ind) %>% 
    filter(treat == "Control") %>% 
    filter(year == 1985)
  
  model = lm(value ~ 1, grnd_df)
  
  confint(model, level = 0.95)
  
  
  condefC = data.frame(grnd_df$index[1], grnd_df$treat[1], grnd_df$year[1], mean(grnd_df$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2]) 
  colnames(condefC) = c("index", "treat", "year", "mean", "low", "upper")
  
  
  grnd_df = df %>% 
    na.omit() %>% 
    filter(index == ind) %>% 
    filter(treat == "Control")
  
  ylp = c(1986:2011, 2013:2019)
  
  
  for (z in ylp) {
    data = grnd_df %>%
      filter(year == z)
    
    model = lm(value ~ 1, data)
    
    
    condefxC = data.frame( grnd_df$index[1], grnd_df$treat[1], z, mean(data$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2])
    colnames(condefxC) = c("index", "treat", "year", "mean", "low", "upper")
    
    condefC = rbind(condefC, condefxC)
    
    print(confint(model, level = 0.95))
  }
  
  condefD = rbind(condef, condefB, condefC)
  return(condefD)
}


conf_evi = desconfidence("EVI")
conf_ndvi = desconfidence("NDVI")
conf_ndii = desconfidence("NDII")
conf_grnd = desconfidence("GRND")
conf_nbr = desconfidence("NBR")
conf_nbr2 = desconfidence("NBR2")

confi = rbind(conf_evi, conf_ndvi, conf_ndii, conf_grnd, conf_nbr, conf_nbr2)


ggplot(confi, aes(x=year, y=mean, color = treat))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2010,linetype = "dashed", col = "gray", size = 1)+
  #geom_line(aes(group = treat), size = 1.5, alpha = 0.8)+
  #geom_ribbon(aes(ymin = low, ymax = upper, fill = treat, col = NULL), alpha = 0.2)+
  geom_errorbar(aes(ymin=low, ymax=upper), width=0)+
  #geom_line(data = df_crt, aes(x=year, y=value, group = index), size = 1.5, alpha = 0.8)+
  geom_point(size = 1, alpha = 0.8)+
  facet_grid(rows = vars(index), scales = "free")+
  labs(y = " ")+
  theme_bw()+
  #geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  scale_color_manual(values = c("orange", "red", "blue"))+
  scale_fill_manual(values = c("orange", "red", "blue"))+
  theme(text = element_text(family = "Times New Roman", size = 14))


#Diference by sbtraction ===========================================
#Calculate difference in relation of control
confi_crt = filter(confi, treat == "Control")
confi_b3yr = filter(confi, treat == "B3yr")
confi_b1yr = filter(confi, treat == "B1yr")

confi_b3yr$value = 100 - ((confi_b3yr$mean*100)/confi_crt$mean)
confi_b3yr$low = 100 - ((confi_b3yr$low*100)/confi_crt$mean)
confi_b3yr$upper = 100 - ((confi_b3yr$upper*100)/confi_crt$mean)

confi_b1yr$value = 100 - ((confi_b1yr$mean*100)/confi_crt$mean)
confi_b1yr$low = 100 - ((confi_b1yr$low*100)/confi_crt$mean)
confi_b1yr$upper = 100 - ((confi_b1yr$upper*100)/confi_crt$mean)


confi_diff = rbind(confi_b3yr, confi_b1yr)
colnames(confi_diff) = c( "Indice", "Treatment", "Year","mean", "low", "upper", "Valor")


difplot = ggplot(confi_diff, aes(x = Year, y = Valor, color = Indice))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Indice), size = 0.5, alpha = 0.8)+
  #geom_line(aes(group = Indice), size = 1.5, alpha = 0.8)+
  #geom_point(size = 1.5, alpha = 0.8)+
  #geom_ribbon(aes(ymin = low, ymax = upper, fill = Indice, color = NULL), alpha = 0.2)+
  geom_errorbar(aes(ymin = low, ymax = upper), width = 1)+
  facet_grid(cols = vars(Treatment), rows = vars(Indice))+
  labs(y = "Burned - Control (% of difference)")+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  scale_color_manual(values = c('#377eb8','#1b9e77','#e41a1c','darkred','#ff7f00','#4daf4a'))+
  scale_fill_manual(values = c('#377eb8','#1b9e77','#e41a1c','darkred','#ff7f00','#4daf4a'))+
  theme(text = element_text(family = "Times New Roman", size = 14)); difplot




ggsave(filename = "Landsat_1985-2019_diff_Confi_Intervals.png", plot = difplot,
      path = "C:/Users/Workshop/Documents/Research/Doutorado/Capitulo1/Figuras/Landsat Time Series", 
     width = 15, height = 20, units =  "cm", dpi = 300)





diff_11 = confi_diff %>% 
  filter(Year == 2011)





























