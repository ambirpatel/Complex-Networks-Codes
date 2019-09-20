celegans <- as.matrix(read.delim("celegans.dat",header = F,sep = ","))
celegans_t <- t(celegans)
cocitation_mat <- celegans%*% celegans_t
bibiliographic_mat <- celegans_t%*%celegans
diag(cocitation_mat) <- 0
diag(bibiliographic_mat) <- 0

View(cocitation_mat)
View(bibiliographic_mat)

#Eigen vector centrality
n <- dim(celegans)[1]
old <- rep(1,n)
new <- old 
eps <- 10^-4

while (sum(abs(old - new)) < eps) {
  old <- new
  for (i in 1:n) {
    x1 <- which(celegans[i,]==1) #neighbor
    x <- sum(old[x1])
    new[i] <- x
  }
  #print(new)
  new <- new/sum(new)
}
Eigen_cent <- new
cat(sprintf("\n Important node wrt Eigen Vector centrality is : %d", which.max(Eigen_cent)))

#Page rank centrality
o <- matrix(1,n)
I <- diag(n)
alpha <- 0.85 #This value is less than max value of eigen value of AD^-1
A <- as.matrix(celegans)
kout <- colSums(A)
kout[kout==0] <- 1
D <- diag(1/kout)
xPR <- solve(I - alpha * A%*%D) %*% o
cat(sprintf("\n Important node wrt Page Rank centrality is : %d", which.max(xPR)))

