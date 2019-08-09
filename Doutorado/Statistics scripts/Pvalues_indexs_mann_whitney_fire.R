## code to run mann whitney test on fire plots to compare median spectral values

library(reshape2)
library(dplyr)
library(ggplot2)

# all the data here
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/INPE (Temporada 2019)/Graficos landsat/Acuracia do Landsat")
df <- read.csv("Master_Landsat_index.csv", header = TRUE, sep = ",")
df$ano = substr(df$ano, 1, 4)


colnames(df) = c('band1', 'band2', 'PLOT', 'year')

# some stuff
plot_combinations = rbind(cbind("controle", "b3yr"), cbind("controle", "b1yr"), cbind("b3yr", "b1yr"))
year_v = c(2000:2011, 2013:2019)

# first we select the band
band = 1
p_value_list = list()
for (band in 1:2) {
  
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

ndvi = as.data.frame(p_value_list[[1]])
evi = as.data.frame(p_value_list[[2]])

ndvi$test = c("control/b3yr","control/b1yr","b3yr/b1yr")
evi$test = c("control/b3yr","control/b1yr","b3yr/b1yr")

gg = melt(ndvi)
colnames(gg) = c('test', 'ano', 'pvalue')
ggplot(gg, aes(ano, pvalue, col=test))+
  geom_line(aes(group=test), size=1)+
  geom_point()+
  labs(title="NDVI")+
  annotate("rect", xmin = 5, xmax = 11, ymin = 0, ymax = Inf, alpha = .2, fill = "red")+
  geom_hline(yintercept = 0.05, color = "black", linetype = "dashed")


gg = melt(evi)
colnames(gg) = c('test', 'ano', 'pvalue')
ggplot(gg, aes(ano, pvalue, col=test))+
  geom_line(aes(group=test), size=1)+
  geom_point()+
  labs(title="EVI")+
  annotate("rect", xmin = 5, xmax = 11, ymin = 0, ymax = Inf, alpha = .2, fill = "red")+
  geom_hline(yintercept = 0.05, color = "black", linetype = "dashed")
