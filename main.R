# Random seed 
set.seed(6046)

library(kernlab)
source("kkmeans.R")

# Load data
ring_data <- read.csv("ring_dataset.csv")
ring.matrix <- as.matrix(ring_data)[,2:3]

# Plot the original data
plot(ring.matrix, col=as.factor(ring_data$label))


# Perform kmeans, and plot result
cluster.kmeans <- kmeans(ring.matrix, 2)
plot(ring.matrix, col=as.factor(cluster.kmeans$cluster))


# Perform kkmeans, and plot result
cluster.kkmeans <- kkmeans(ring.matrix, 2, kernel='rbfdot')
plot(ring.matrix, col=as.factor(cluster.kkmeans@.Data))


# Perform own kkmeans, and plot result

# Initialize with the results from Kmeans
ini.clusters <- cluster.kmeans$cluster
ini.ohencoding <- ohencoding_cluster(ini.clusters, 2)

# Initialize with random clustering
random.ini <- random_cluster(nrow(ring.matrix), 2)

K <- kernelMatrix(rbfdot(), ring.matrix, y = NULL)
cluster.kkmenas_own <- kkmeans_own(K, 2, random.ini, nrow(K))
plot(ring.matrix, col=as.factor(cluster_labels(random.ini),2))
plot(ring.matrix, col=as.factor(cluster_labels(cluster.kkmenas_own, 2)))


random.ini


