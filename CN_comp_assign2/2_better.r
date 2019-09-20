lesmis <- read.delim("lesmis.dat", header = F, sep = ",")
d <- data.frame((lesmis[,1] + 1), (lesmis[,2] + 1)) 
n <- max(d) #No. of vertices
a <- matrix(0,n,n,dimnames = list(c(seq(1:n)),c(seq(1:n))))

#Adjacency matrix from given list
for (i in 1:nrow(d)){
  x <- d[i,1] 
  y <- d[i,2]
  a[x,y] <- 1
  a[y,x] <- 1
}

#Adjacency lists
adj_list <- NULL
for (i in 1:n) {
  adj_list[[i]] <- which(a[i,]==1)
}

#BFS better Implementation
dist <- matrix(-1, n, n, byrow = T,dimnames = list(c(seq(1:n)),c(seq(1:n)))) #Distance array for each vertex
start_time <- Sys.time()

for (i in 1:n) { #Running BFS n times
  dist[i,i] <- 0  #Set source dist 0
  queue <- NULL
  queue <- c(queue,i)
  j <- 1
  
  while(length(which(dist[i,] == -1))!=0){
    idx <- queue[j:length(queue)]
    nridx <- length(idx)
    for (j in 1:nridx) {
      d <- dist[i,idx[j]]
      nbr <- adj_list[[idx[j]]]
      for (k in nbr) { 
        if(dist[i,k] == -1){
          dist[i,k] <- d + 1 
          queue <- c(queue,k)
        }
      }
      j <- j + 1
    }
  }
}
end_time <- Sys.time()
time_req <- difftime(end_time, start_time)
cat(sprintf("Time required for better Implementation is : %f seconds\n",time_req))
