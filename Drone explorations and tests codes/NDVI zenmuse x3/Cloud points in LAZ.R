library(lidR)

a = readLAS(choose.files())
plot(a)

#View(a@data)

df = as.data.frame(a@data)


