######################################
# Ecological exploration of blowdown #
#                                    #
# Eduardo Q Marques 10-03-2022       #
######################################

library(tidyverse)
library(reshape2)

#First Part =================================================================================
#Merged field data
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento")

df = read.csv("blowdown_full_update_2021.csv", sep = ",")

#Abundance of time series -------------------------------------------------------------------
df04 = df %>% 
  filter(dap.04 != "NA")
df04 = length(df04$tipo_de_dano)

#df06 = df %>% 
 # filter(dap.06 != "NA")
#df06 = length(df06$tipo_de_dano)

#df07 = df %>% 
 # filter(dap.07 != "NA")
#df07 = length(df07$tipo_de_dano)

df08 = df %>% 
  filter(dap.08 != "NA")
df08 = length(df08$tipo_de_dano)

df10 = df %>% 
  filter(dap.10 != "NA")
df10 = length(df10$tipo_de_dano)

df11 = df %>% 
  filter(dap.11 != "NA")
df11 = length(df11$tipo_de_dano)

df12 = df %>% 
  filter(dap.12 != "NA")
df12 = length(df12$tipo_de_dano)

df14 = df %>% 
  filter(dap.14 != "NA")
df14 = length(df14$tipo_de_dano)

df16 = df %>% 
  filter(dap.16 != "NA")
df16 = length(df16$tipo_de_dano)

df18 = df %>% 
  filter(dap.18 != "NA")
df18 = length(df18$tipo_de_dano)


abu <- data.frame(Abundance=c(df04, df08, df10, df11, df12, df14, df16, df18),
                 Year=c(2004, 2008, 2010, 2011, 2012, 2014, 2016, 2018))

ggplot(abu, aes(x = Year, y = Abundance))+
  geom_col()














