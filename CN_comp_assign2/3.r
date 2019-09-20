polbooks <- read.delim("polbooks.dat", header = F, sep = ",")
d <- data.frame((polbooks[,1] + 1), (polbooks[,2] + 1)) 
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

#BFS Naive Implementation
dist <- matrix(-1, n, n, byrow = T,dimnames = list(c(seq(1:n)),c(seq(1:n)))) #Distance array for each vertex
pred <- matrix(-1, n, n, byrow = T,dimnames = list(c(seq(1:n)),c(seq(1:n)))) #Predecessor array for shortest path between every vertex pair

for (i in 1:n) { #Running BFS n times
  d <- 0  #Initialize dist 
  dist[i,i] <- 0  #Set source dist 0
  pred[i,i] <- 0
  
  while(length(which(dist[i,] == -1))!=0){
    o <- which(dist[i,] == d)  #vertices with dist d
    for (j in o) {
      x <- adj_list[[j]]  #neighbors of vertex with dist d
      for (k in x) { 
        if(dist[i,k] == -1){
          dist[i,k] <- d + 1 
          if(pred[i,k] == -1) { pred[i,k] <- j } #Creating predecessor array
        } 
      }
    }
    d <- d + 1 
  }
}

#Shortest path between Given vertices
a <- as.numeric(readline(prompt="Enter an vertex 1: "))
b <- as.numeric(readline(prompt="Enter an vertex 2: "))
print(b)
repeat{
  t <- pred[a,b]
  if(t==a){break}
  print(t)
  b <- t
}
print(a)
