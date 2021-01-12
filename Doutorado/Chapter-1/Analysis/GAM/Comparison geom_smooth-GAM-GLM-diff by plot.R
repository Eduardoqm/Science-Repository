#Comparison geom_smooth-GAM-GLM-diff by plot

#Eduardo Q Marques 11-12-2020

library(tidyverse)
library(reshape2)
library(ggplot2)
library(mgcv)
library(visreg)

#Data ============================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Modify data =====================================================
df$year = substr(df$year, 1,4)
df$year = as.numeric(df$year)

#NDVI
ndvi = df %>% 
  filter(index == "ndvi")

eqm = c("red", "orange", "blue")

#GGPLOT smooth GAM ===============================================
ggplot(ndvi, aes(x=year, y=value, color = treat))+
  #geom_rect(aes(xmin = 2004, xmax = 2011, ymin = 0.72, ymax = Inf),
   #         fill = "red", color = NA, alpha = 0.01)+
  #annotate("text", x = 2007.5, y = 0.8, size = 4, label = "Fire experiment period")+
  #geom_point(alpha = 0.1)+
  geom_line(aes(x = year, y = mean(value), group = treat))+
  #geom_smooth(aes(group = treat), size = 1.5, alpha = 0.7)+
  theme_light()+
  scale_color_manual(values = eqm)+
  ggtitle("NDVI-geom_smooth")
  #theme(axis.text.x = element_text(angle = 90))

#GAM from mgcv ===================================================
ndvig = gam(value~s(year, k = 33, by = treat), data = ndvi, method = "REML", family = binomial("identity"))

summary(ndvig)
#                        edf Ref.df  Chi.sq p-value    
#s(year):treatb1yr     7.406  9.224 161.833  <2e-16 ***
#s(year):treatb3yr    22.260 26.400 269.208  <2e-16 ***
#s(year):treatcontrol  1.004  1.007   0.181   0.673  

#gam.check(ndvig)
#                        k'   edf k-index p-value  
#s(year):treatb1yr    32.00  7.41    0.97   0.025 *
#s(year):treatb3yr    32.00 22.26    0.97   0.025 *
#s(year):treatcontrol 32.00  1.00    0.97   0.015 *

plot(ndvig, pages = 1, shade = T, shade.col = "orange",
     shift = coef(ndvig)[1]) #With values of ndvi


a = visreg(ndvig, "year", by = "treat", overlay = T,
           partial = F)

ggplot(a$fit, aes(year, visregFit, col = treat))+
  #geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
   #         fill = "red", color = NA, alpha = 0.005)+
  #annotate("text", x = 2007.5, y = 0.85, size = 4, label = "Fire experiment period")+
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.1) +
  geom_line(size = 1.5, aplha = 0.8)+
  scale_color_manual(values = eqm)+
  ggtitle("NDVI-GAM")+
  ylab("Index")+
  theme_light()

#GLM ===============================================================
ndvi_glm = glm(value~year+treat, data = ndvi, family=poisson())

summary(ndvi_glm)
plot(ndvi_glm)

#Diference by sbtraction ===========================================
ndvi_m = ndvi %>% 
  group_by(year, treat) %>% 
  summarise(value = mean(value))

ggplot(ndvi_m, aes(x=year, y=value, color = treat))+
  geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            fill = "blue", color = NA, alpha = 0.005)+
  annotate("text", x = 2007.5, y = 0.85, size = 4, label = "Fire experiment period")+
  geom_line(aes(group = treat), size = 1.5, alpha = 0.8)+
  theme_light()+
  scale_color_manual(values = eqm)+
  ggtitle("NDVI-mean")
  #theme(axis.text.x = element_text(angle = 90))

#Calculate difference in relation of control
ndvi_crt = filter(ndvi_m, treat == "control")
ndvi_b3yr = filter(ndvi_m, treat == "b3yr")
ndvi_b1yr = filter(ndvi_m, treat == "b1yr")

ndvi_b3yr$value = ndvi_b3yr$value - ndvi_crt$value
ndvi_b1yr$value = ndvi_b1yr$value - ndvi_crt$value
ndvi_diff = rbind(ndvi_b3yr, ndvi_b1yr)

ggplot(ndvi_diff, aes(x=year, y=value, color = treat))+
  geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            fill = "blue", color = NA, alpha = 0.01)+
  annotate("text", x = 2007.5, y = 0.01, size = 4, label = "Fire experiment period")+
  geom_line(aes(group = treat), size = 1.5, alpha = 0.8)+
  theme_light()+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = eqm)+
  ggtitle("NDVI-difference by control")
  #theme(axis.text.x = element_text(angle = 90))

