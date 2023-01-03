#---------------------------------------------------------------------------
# Data Frame from optimal Temp/Space window combination V2 (Dataframes)
#
# Eduardo Q Marques 11-10-2022
#----------------------------------------------------------------------------

# ABOUT: For some motivation the CHI calculation from rasters do not input the
# 2012-2020 series. Because this problem I run the 2012-2020 separeted, and here
# I'm join and recalulating the CHI on 0.9 quantile to extract Optimal CHI and
# the Optimal Window, to compare with vegetation Hight.

library(tidyverse)
library(reshape2)
library(extRemes)

#Load data ------------------------------------------------------------------
setwd("/home/queirozm/eqm_eth_ufz/Data/DataFrames")
era1 = read.csv("ALL_ERA5_Block_combination_1979_2011.csv", sep = ",")
era2 = read.csv("ALL_ERA5_Block_combination_2012_2020.csv", sep = ",")

#Join the incomplete timeseries
df = rbind(era1, era2)
df2 = df[,-8]

#setwd("/home/queirozm/eqm_eth_ufz/Data/DataFrames")
#write.csv(df2, "ALL_ERA5_Block_combination_1979_2020.csv", row.names = F)

#Process before calculating CHI ----------------------------------------------
df2 = df2 %>% unite("xy", x:y, sep = "_")
df = df %>% unite("xy", x:y, sep = "_")

#List to loop CHI calculations for each pixel
xylist = df %>%
  group_by(xy) %>%
  summarize(CHI = mean (CHI))

#Separate data in surronding pixels (window)
p1 = df2 %>% filter(degrees == "0.25°")
p3 = df2 %>% filter(degrees == "0.75°")
p5 = df2 %>% filter(degrees == "1.25°")
p7 = df2 %>% filter(degrees == "1.75°")


#Calculating CHI and otimal windows at 0.9 quantile --------------------------
#Function to calculate
pixies = function(z){
    multd = z

    multd1 = multd %>% filter(window == "1d") %>% filter(xy == c(xylist[1,1]))
    multd3 = multd %>% filter(window == "3d") %>% filter(xy == c(xylist[1,1]))
    multd5 = multd %>% filter(window == "5d") %>% filter(xy == c(xylist[1,1]))
    
    t1 = taildep(multd1$Prec, multd1$Wind, 0.9)
    t3 = taildep(multd3$Prec, multd3$Wind, 0.9)
    t5 = taildep(multd5$Prec, multd5$Wind, 0.9)

    multd1$CHI = t1[[1]]
    multd3$CHI = t3[[1]]
    multd5$CHI = t5[[1]]

    multdfull = rbind(multd1, multd3, multd5)

    for(abc in 2:length(xylist$xy)){
      print(abc)
      multd1 = multd %>% filter(window == "1d") %>% filter(xy == c(xylist[abc,1]))
      multd3 = multd %>% filter(window == "3d") %>% filter(xy == c(xylist[abc,1]))
      multd5 = multd %>% filter(window == "5d") %>% filter(xy == c(xylist[abc,1]))
    
      t1 = taildep(multd1$Prec, multd1$Wind, 0.9)
      t3 = taildep(multd3$Prec, multd3$Wind, 0.9)
      t5 = taildep(multd5$Prec, multd5$Wind, 0.9)

      multd1$CHI = t1[[1]]
      multd3$CHI = t3[[1]]
      multd5$CHI = t5[[1]]

      multdfull2 = rbind(multd1, multd3, multd5)
      multdfull = rbind(multdfull, multdfull2)
    }

    return(multdfull)
}

#Calculating
p1b = pixies(p1)
p3b = pixies(p3)
p5b = pixies(p5)
p7b = pixies(p7)

#Join results
df3 = rbind(p1b, p3b, p5b, p7b)
df3 = df3 %>% separate(xy, c("x","y"), sep = "_")

setwd("/home/queirozm/eqm_eth_ufz/Data/DataFrames")
write.csv(df3, "ALL_ERA5_Block_combination_1979_2020.csv", row.names = F)
write.csv(p1b, "ERA5_Block_1pixel_1979_2020.csv", row.names = F)
write.csv(p3b, "ERA5_Block_3pixel_1979_2020.csv", row.names = F)
write.csv(p5b, "ERA5_Block_5pixel_1979_2020.csv", row.names = F)
write.csv(p7b, "ERA5_Block_7pixel_1979_2020.csv", row.names = F)

#Extract Optimal CHI
df4 = df3 %>%
  group_by(x, y, date) %>%
  slice(which.max(CHI))%>%
  unite("Class", window, degrees, sep = ", ") %>%
  ungroup()

df4 = df4 %>%
    filter(date != "1979-01-01") %>%
    filter(date != "1979-01-02")
df4 = df4[,-2]

write.csv(df4, "Optimal_ERA5_Block_1979_2020.csv", row.names = F)