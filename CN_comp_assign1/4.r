#Incidence matrix for given bipartite n/w
B <- matrix(c(1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,1),byrow = T, 4, 7,
            dimnames = list(c('A','B','C','D'),c(seq(1:7))))
print("Incidence matrix is")
show(B)

#Adjacency matrix for projections
#1.Vertex projection
VB <- B %*% t(B)
print("Adj matrix for vertex projection")
show(VB)

#2.Group projection
GB <- t(B) %*% B
print("Adj matrix for group projection")
show(GB)