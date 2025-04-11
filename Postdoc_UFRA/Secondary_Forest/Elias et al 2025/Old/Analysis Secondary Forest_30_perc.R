#Analysis Secondary Forest > 30% (Elias Paper)

#Eduardo Q Marques 13-03-2025

library(tidyverse)
library(lme4)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Eduardo/Documents/Analises_Elias/Dados")
df = read.csv("Paper_Elias_Data.csv")

#Preparing data to analysis ----------------------------------------------------
df2=df%>%
  na.omit() %>% 
  dplyr::filter(LST>=24) %>% 
  group_by(Regions, Age_secforest) %>% 
  filter(Age_secforest >4) %>% 
  filter(Age_secforest <36) %>% 
  summarise(LST = mean(LST),
            ET = mean(ET),
            Perc_agriculture = mean(Perc_agriculture),
            Perc_priforest = mean(Perc_priforest),
            Perc_secforest = mean(Perc_priforest))

#Exploratory Graphics ----------------------------------------------------------
ggplot(df2, aes(x = Age_secforest, y = LST))+
  geom_point(aes(colour = Perc_agriculture))+
  geom_smooth(method = "lm")+
  facet_wrap(Regions~.,scales = 'free')+
  scale_color_gradient(low='blue',high = 'yellow')+
  theme_minimal()

ggplot(df2, aes(x = Age_secforest, y = ET))+
  geom_point(aes(colour = Perc_agriculture))+
  geom_smooth(method = "lm")+
  facet_wrap(Regions~.,scales = 'free')+
  scale_color_gradient(low='blue',high = 'yellow')+
  theme_minimal()

#Models ------------------------------------------------------------------------
#LST Model
m_lst=lmer(LST~Age_secforest*Regions+(1|Perc_agriculture)+(1|Perc_priforest),data=df2,
           control=lmerControl(check.nobs.vs.nlev = "ignore",
                               check.nobs.vs.rankZ = "ignore",
                               check.nobs.vs.nRE="ignore"))

summary(m_lst)
MuMIn::r.squaredGLMM(m_lst)

df2$pred_lst=predict(m_lst)

ggplot(df2, aes(x = Age_secforest, y = LST))+
  geom_point(aes(colour = Perc_agriculture))+
  geom_smooth(aes(x=Age_secforest, y=pred_lst))+
  facet_wrap(Regions~.,scales = 'free')+
  scale_color_gradient(low='blue',high = 'yellow')+
  theme_minimal()


#ET Model
m_et=lmer(ET~Age_secforest*Regions+(1|Perc_agriculture)+(1|Perc_priforest),data=df2,
          control=lmerControl(check.nobs.vs.nlev = "ignore",
                              check.nobs.vs.rankZ = "ignore",
                              check.nobs.vs.nRE="ignore"))
summary(m_et)
MuMIn::r.squaredGLMM(m_et)

df2$pred_et=predict(m_et)

ggplot(df2, aes(x = Age_secforest, y = ET))+
  geom_point(aes(colour = Perc_agriculture))+
  geom_smooth(aes(x=Age_secforest, y=pred_et))+
  facet_wrap(Regions~.,scales = 'free')+
  scale_color_gradient(low='blue',high = 'yellow')+
  theme_minimal()
