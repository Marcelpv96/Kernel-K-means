# This is the difference between the new kernel approach and normal approach. 
# In kernels you can not obtain  the centroid of data in feature space
get_distance <- function(K, n, k, I){
  dist <- K[n,n] - 2*I[,k] %*% K[,n] / sum(I[,k]) 
  numerator <- sum(I[,k] * K %*% I[,k]) 
  denominator <- (I[,k] %*% I[,k])**2
  dist <- dist + numerator/denominator
  return(dist)
}



# Function own kkmeans
kkmeans_own <- function(K, c, clusters, N){
  new.clusters <- matrix(nrow=N, ncol=c) # list of new clusters 
  start_time <- Sys.time()
  for (n in 1:N){
    distances <- c()
    for (i in 1:c){ # For each cluster obtain which distance are this point 'n' to him.
      distances <- c(distances, get_distance(K, n, i, clusters))
    }
    new.clusters[n,] <- as.numeric(distances == min(distances))
  }
  end_time <-Sys.time()
  print(end_time-start_time)
  if(identical(new.clusters, clusters)){
    return(new.clusters)
  }else{
    kkmeans_own(K, c, new.clusters, N)
  }
}


# Function random cluster initialization
random_cluster <- function(N, num_clusters){
  clusters <- matrix(0,nrow=N, ncol=num_clusters)
  for (k in 1:num_clusters){
    i <- sample(N, 1)
    while(sum(clusters[i,])>0){
      i <- sample(N, 1)
    }
    clusters[i,k] <-1 
  }
  return(clusters)
}


# Function one hot encoding cluster
ohencoding_cluster <- function(clusters, num_clusters){
  dummy_variables <- matrix(nrow=length(clusters), ncol = num_clusters)
  for (k in 1:num_clusters){
    dummy_variables[,k] <- as.numeric(clusters==k)
  }
  return(dummy_variables)
}


# Function obtain labels from onehot encoding
cluster_labels <- function(dummy_variables, num_clusters){
  clusters <- c()
  for(i in 1:nrow(dummy_variables)){
    clusters <- c(clusters, which(dummy_variables[i,] == 1))
  }
  return(clusters)
}


# Function to obtain the accuracy
accuracy <- function(prediction, real){
  success <- 0
  for(i in 1:length(prediction)){
    if (prediction[i] == real[i]) {
      success <- success + 1
    }
  }
  accuracy <- success / length(prediction)
  accuracy <- max(accuracy, 1-accuracy)
  return(accuracy)
}





