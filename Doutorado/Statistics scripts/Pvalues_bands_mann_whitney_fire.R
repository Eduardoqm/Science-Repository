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

blue$test = c("control/b3yr","control/b1yr","b3yr/b1yr")
green$test = c("control/b3yr","control/b1yr","b3yr/b1yr")
red$test = c("control/b3yr","control/b1yr","b3yr/b1yr")
irprox$test = c("control/b3yr","control/b1yr","b3yr/b1yr")
irmedio$test = c("control/b3yr","control/b1yr","b3yr/b1yr")
irmedio2$test = c("control/b3yr","control/b1yr","b3yr/b1yr")

gg = melt(blue)
colnames(gg) = c('test', 'ano', 'pvalue')
ggplot(gg, aes(ano, pvalue, col=test))+
  geom_line(aes(group=test), size=1)+
  geom_point()+
  labs(title="Banda Azul")+
  annotate("rect", xmin = 5, xmax = 11, ymin = 0, ymax = Inf, alpha = .2, fill = "red")+
  geom_hline(yintercept = 0.05, color = "black", linetype = "dashed")


gg = melt(green)
colnames(gg) = c('test', 'ano', 'pvalue')
ggplot(gg, aes(ano, pvalue, col=test))+
  geom_line(aes(group=test), size=1)+
  geom_point()+
  labs(title="Banda Verde")+
  annotate("rect", xmin = 5, xmax = 11, ymin = 0, ymax = Inf, alpha = .2, fill = "red")+
  geom_hline(yintercept = 0.05, color = "black", linetype = "dashed")


gg = melt(red)
colnames(gg) = c('test', 'ano', 'pvalue')
ggplot(gg, aes(ano, pvalue, col=test))+
  geom_line(aes(group=test), size=1)+
  geom_point()+
  labs(title="Banda Vermelho")+
  annotate("rect", xmin = 5, xmax = 11, ymin = 0, ymax = Inf, alpha = .2, fill = "red")+
  geom_hline(yintercept = 0.05, color = "black", linetype = "dashed")


gg = melt(irprox)
colnames(gg) = c('test', 'ano', 'pvalue')
ggplot(gg, aes(ano, pvalue, col=test))+
  geom_line(aes(group=test), size=1)+
  geom_point()+
  labs(title="IR proximo")+
  annotate("rect", xmin = 5, xmax = 11, ymin = 0, ymax = Inf, alpha = .2, fill = "red")+
  geom_hline(yintercept = 0.05, color = "black", linetype = "dashed")


gg = melt(irmedio)
colnames(gg) = c('test', 'ano', 'pvalue')
ggplot(gg, aes(ano, pvalue, col=test))+
  geom_line(aes(group=test), size=1)+
  geom_point()+
  labs(title="IR medio")+
  annotate("rect", xmin = 5, xmax = 11, ymin = 0, ymax = Inf, alpha = .2, fill = "red")+
  geom_hline(yintercept = 0.05, color = "black", linetype = "dashed")


gg = melt(irmedio2)
colnames(gg) = c('test', 'ano', 'pvalue')
ggplot(gg, aes(ano, pvalue, col=test))+
  geom_line(aes(group=test), size=1)+
  geom_point()+
  labs(title="IR medio2")+
  annotate("rect", xmin = 5, xmax = 11, ymin = 0, ymax = Inf, alpha = .2, fill = "red")+
  geom_hline(yintercept = 0.05, color = "black", linetype = "dashed")
