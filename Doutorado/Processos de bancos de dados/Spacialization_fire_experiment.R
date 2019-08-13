#Spacilization fire
#Eduaro Q Marques  13/08/2019

library(ggplot2); library(reshape2); library(dplyr); library(tidyr); library(rgdal)

#Fire data -----------------------------------------------------------------------------------
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Area1-plot/Fogo')
fire <- read.csv("FIRE DATA SUMMARY.csv", header = TRUE, sep = ",")
#View(fire)

#Manipulate data
fire <- fire[,c(2, 3, 9)]

fire <- filter(fire, YR==2004)
fire = fire %>% 
  separate(col = "id", c("id", "ano"), sep = ' ')
fire <- fire[,c(1, 2, 4)]

fire2 = fire %>% 
  group_by(id) %>%
  na.omit() %>%
  summarise(alt..cm. = mean(alt..cm.))

colnames(fire2) = c("id2", "alt_fire")

#Shape do input data --------------------------------------------------------------------------
grid <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Grid_Area1",layer="Grid_Area1")

grid_df <-  data.frame(grid)
grid_df = grid_df %>% 
  unite(col = "id2", c("Transc", "Line"), sep = '')
grid@data$id2 = grid_df$id2

#Insert data on the shape
grid_df = merge(grid_df, fire2, by = "id2", all = F)

grid@data = merge(grid@data, grid_df, by = "id2", all = T)

grid@data[is.na(grid@data)] <- 0

#See shape and data frame
View(grid@data)

plot(grid)

spplot(grid["alt_fire"])
















