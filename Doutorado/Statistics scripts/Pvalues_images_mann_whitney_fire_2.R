## code to run mann whitney test on fire plots to compare median spectral values

library(reshape2)
library(dplyr)
library(ggplot2)

# all the data here
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/INPE (Temporada 2019)/Graficos landsat/Acuracia do Landsat")
df <- read.csv("Master_Landsat_bands.csv", header = TRUE, sep = ",")
df$ano = substr(df$ano, 1, 4)


colnames(df) = c('band1', 'band2', 'band3', 'band4', 'band5', 'band7', 'PLOT', 'year')

# some stuff
plot_combinations = rbind(cbind("controle", "b3yr"), cbind("controle", "b1yr"), cbind("b3yr", "b1yr"))
year_v = c(2000:2011, 2013:2019)

# first we select the band
band = 1
p_value_list = list()
for (band in 1:6) {
  
  p_value_list[[band]] = matrix(0, nrow = 3, ncol = 19)
  colnames(p_value_list[[band]]) = c(2000:2011, 2013:2019)
  rownames(p_value_list[[band]]) = c("controle/b3yr","controle/b1yr","b3yr/b1yr")
  
  # then we select the year
  year = 1
  for (year in 1:length(year_v)) {
    df_year = df[df$year == year_v[year], ]
    
    # define the variables
    y = df_year[,band]
    A = df_year$PLOT
    
    # select the plots to test
    plot_test = 1
    for (plot_test in 1:3) {
      # apply filter  
      y_filtered = y[A %in% plot_combinations[plot_test,]]
      A_filtered = A[A %in% plot_combinations[plot_test,]]
      
      # apply test
      wt = wilcox.test(y_filtered ~ A_filtered)
      p_value_list[[band]][plot_test, year] = wt$p.value
      
    }
    
  }
  
  
}

blue = as.data.frame(p_value_list[[1]])
green = as.data.frame(p_value_list[[2]])
red = as.data.frame(p_value_list[[3]])
irprox = as.data.frame(p_value_list[[4]])
irmedio = as.data.frame(p_value_list[[5]])
irmedio2 = as.data.frame(p_value_list[[6]])



colnames(blue) = c("plot")



ggplot(blue)




band=1
plot(year_v, p_value_list[[band]][1,], type="l", ylim=c(0,1), col="green")
lines(year_v, p_value_list[[band]][2,], col = "blue")
lines(year_v, p_value_list[[band]][3,], col = "red")
legend("topright", legend = c("crt/b3yr","crt/b1yr","b3yr/b1yr"), lty = "solid", col=c("green","blue","red"))


band=2
plot(year_v, p_value_list[[band]][1,], type="l", ylim=c(0,1), col="green")
lines(year_v, p_value_list[[band]][2,], col = "blue")
lines(year_v, p_value_list[[band]][3,], col = "red")
legend("topright", legend = c("crt/b3yr","crt/b1yr","b3yr/b1yr"), lty = "solid", col=c("green","blue","red"))

band=3
plot(year_v, p_value_list[[band]][1,], type="l", ylim=c(0,1), col="green")
lines(year_v, p_value_list[[band]][2,], col = "blue")
lines(year_v, p_value_list[[band]][3,], col = "red")
legend("topright", legend = c("crt/b3yr","crt/b1yr","b3yr/b1yr"), lty = "solid", col=c("green","blue","red"))

band=4
plot(year_v, p_value_list[[band]][1,], type="l", ylim=c(0,1), col="green")
lines(year_v, p_value_list[[band]][2,], col = "blue")
lines(year_v, p_value_list[[band]][3,], col = "red")
legend("topright", legend = c("crt/b3yr","crt/b1yr","b3yr/b1yr"), lty = "solid", col=c("green","blue","red"))

band=5
plot(year_v, p_value_list[[band]][1,], type="l", ylim=c(0,1), col="green")
lines(year_v, p_value_list[[band]][2,], col = "blue")
lines(year_v, p_value_list[[band]][3,], col = "red")
legend("topright", legend = c("crt/b3yr","crt/b1yr","b3yr/b1yr"), lty = "solid", col=c("green","blue","red"))

band=6
plot(year_v, p_value_list[[band]][1,], type="l", ylim=c(0,1), col="green")
lines(year_v, p_value_list[[band]][2,], col = "blue")
lines(year_v, p_value_list[[band]][3,], col = "red")
legend("topright", legend = c("crt/b3yr","crt/b1yr","b3yr/b1yr"), lty = "solid", col=c("green","blue","red"))

