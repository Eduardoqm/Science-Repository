#########################################################
# Skewness and Kurtosis of Vegetation Indices (Landsat) #
#                                                       #
# Eduardo Q Marques 24-11-2020                          #
#########################################################

library(ggplot2)
library(e1071)

#Load Data
data = as.data.frame(c(1:100))
data$values = rnorm(100)
colnames(data) = c("x", "values")

#Skewness and Kurtosis calculation
sks = skewness(data$values)
curt = kurtosis(data$values)

sks
curt

#Plot results
ggplot(data, aes(values))+
  geom_density(fill = "blue", alpha = 0.5)+
  geom_vline(xintercept = sks, color = "red", size = 1)+
  geom_vline(xintercept = curt, color = "black", size = 1)
