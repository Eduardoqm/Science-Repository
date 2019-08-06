## code to run mann whitney test on fire plots to compare median spectral values

# all the data here
df = data.frame(B1 = c(1:5), B2 = c(2:6), PLOT = c("A","B","C","B","C"), year = c(2000,2000,2000,2000,2001))

# some stuff
plot_combinations = rbind(cbind("A", "B"), cbind("A", "C"), cbind("B", "C"))
p_value_list = list()
year_v = 2000:2019

# first we select the band
band = 1
for (band in 1:2) {
  
  p_value_list[[band]] = matrix(0, nrow = 3, ncol = 20)
  colnames(p_value_list[[band]]) = 2000:2019
  rownames(p_value_list[[band]]) = c("A/B","A/C","B/C")
  
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
