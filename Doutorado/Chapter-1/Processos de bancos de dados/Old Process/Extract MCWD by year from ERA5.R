#Extract MCWD by year from ERA5
library(ggplot2); library(reshape2); library(dplyr)

setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Climate data')
clima <- read.csv("ERA5_Tanguro_1979_2018.csv", header = TRUE, sep = ",")
View(clima2)

clima2 <- clima[,c(4,13)]
clima2 = clima2 %>% 
  group_by(year) %>% 
  summarise(mcwd = mean(mcwd))
clima2 <- clima2[-c(1:21),]
mcwd <- clima2[-c(13, 20),]
