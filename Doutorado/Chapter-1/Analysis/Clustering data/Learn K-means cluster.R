#Learn K-means cluster

#Eduardo Q Marques 30-01-2020

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

df <- USArrests
df <- na.omit(df)
df <- scale(df)

#Compute and visualize the distance matrix
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "blue", mid = "white", high = "red"))

#Computing k-means clustering in R
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
#cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
#centers: A matrix of cluster centers.
#totss: The total sum of squares.
#withinss: Vector of within-cluster sum of squares, one component per cluster.
#tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
#betweenss: The between-cluster sum of squares, i.e. $totss-tot.withinss$.
#size: The number of points in each cluster.

#PCA by cluster
#Plot one
fviz_cluster(k2, data = df)

#Plot two
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()


#Trying more numbers of cluster (k)
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#Calculate ideal numebers of cluster (k)
fviz_nbclust(df, kmeans, method = "silhouette")

#Final results vizualization
final <- kmeans(df, 2, nstart = 25)
print(final)

fviz_cluster(final, data = df)
























