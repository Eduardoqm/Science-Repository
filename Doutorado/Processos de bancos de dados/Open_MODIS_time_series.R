a = read.csv("D:\\Downloads\\ee-chart (1).csv")
View(a)

as.numeric(gsub(",", "", a$NDVI))