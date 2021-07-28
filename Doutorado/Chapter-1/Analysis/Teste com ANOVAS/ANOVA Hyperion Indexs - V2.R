#ANOVA of Hyperion Indexs - V2

#Eduardo Q Marques 27-07-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(car)

#Data preparation ------------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

df$year = as.factor(df$year)

df2 = df %>% 
  filter(index == "vig") %>%
  na.omit()

ggboxplot(df2, x = "year", y = "value",
          color = "treat", palette = "jco")

#Test normality and homogenity -----------------------------------------------------------
shapiro.test(df2$value)
leveneTest(value~year, data = df2)

#Create QQ plot for each cell of design
ggqqplot(df2, "value", ggtheme = theme_bw()) +
  facet_grid(treat ~ year, labeller = "label_both")

#Run ANOVA -------------------------------------------------------------------------------
model <- aov(value~year:treat, data = df2, paried = F)
model; summary.aov(model)

#Plotting residuals
layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))
plot(model); hist(model$residuals)


#PostHoc analysis ------------------------------------------------------------------------
posth = TukeyHSD(model, wich = "year")

#Convert to dataframe and plot
posth2 = as.data.frame(posth[["year:treat"]])
posth2$comp = row.names(posth2)
posth2 = remove_rownames(posth2)
colnames(posth2)[4] = c("pvalue")

ggplot(posth2, aes(x=comp, y=pvalue))+
  geom_point(size = 1.5)


#Filter interesting comparisons (control and burned treatments)
posth2$treat = substr(posth2$comp, 19, 22)
posth2$year = substr(posth2$comp, 14, 17)

posth3 = posth2 %>% 
  filter(comp %in% c("2004:control-2004:b1yr", "2004:control-2004:b3yr",
                     "2005:control-2005:b1yr", "2005:control-2005:b3yr",
                     "2006:control-2006:b1yr", "2006:control-2006:b3yr",
                     "2008:control-2008:b1yr", "2008:control-2008:b3yr",
                     "2010:control-2010:b1yr", "2010:control-2010:b3yr",
                     "2011:control-2011:b1yr", "2011:control-2011:b3yr",
                     "2012:control-2012:b1yr", "2012:control-2012:b3yr"))


ggplot(posth3, aes(x=year, y=pvalue, color = treat, shape = treat))+
  geom_point(size = 10, alpha = 0.7)+
  geom_hline(yintercept = 0.05, linetype = "dashed")+
  scale_color_manual(values = c("darkorange", "red"))+
  theme_bw()





#Include results of significance in full data frame -------------------------------------
df$diff = c("ns") #ns = Not significant

df$diff[df$index == "ndvi", df$year = ]

















