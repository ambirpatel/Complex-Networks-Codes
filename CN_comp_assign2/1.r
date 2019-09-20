polbooks <- read.delim("polbooks.dat", header = F, sep = ",")
lesmis <- read.delim("lesmis.dat", header = F, sep = ",")
#d <- data.frame((lesmis[,1] + 1), (lesmis[,2] + 1)) 
d <- data.frame((polbooks[,1] + 1), (polbooks[,2] + 1)) 
n <- max(d) #No. of vertices
a <- matrix(0,n,n,dimnames = list(c(seq(1:n)),c(seq(1:n))))

#Creating adjacency matrix from given list
for (i in 1:nrow(d)){
  x <- d[i,1] 
  y <- d[i,2]
  a[x,y] <- 1
  a[y,x] <- 1
}

#Degree
degree <- function(x){return(as.numeric(rowSums(a)[x]))}

#Global clustering coefficient
T <- 0 #number of triples
for (i in 1:n) {
  k <- degree(i)
  t <- (k * (k - 1))/2
  T <- t + T
}
tr <- sum(diag(a %*% a %*% a))/6 #number of triangles

C_gcc <-(tr/T) *3 #Global clust. coeff.
cat(sprintf("Global clustering coefficient is : %f \n",C_gcc))

#Degree assortativity
c <- sum(rowSums(a))/n   #Avg degree
m <- (n*c)/2  #No. of edges
R1 <- NULL
R2 <- NULL
for (i in 1:n) {
  for (j in 1:n) {
    if(i == j){d = 1} #Kronecker delta
    if(i != j){d = 0}
    r1 <- (a[i,j] - (degree(i)*degree(j))/(2*m))*(degree(i)*degree(j))
    r2 <- ((degree(i)*d) - (degree(i)*degree(j))/(2*m))*(degree(i)*degree(j))
    R1 <- c(r1, R1)
    R2 <- c(r2, R2)
  }
}
r <- sum(R1)/sum(R2)  #Degree assort.
cat(sprintf("Degree assortativity coefficient is : %f \n",r))
