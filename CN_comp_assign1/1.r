#Adjacency matrix1 i/p based on given undirected graph
adj_mat1 <- matrix(c(0,1,0,0,3,0,1,2,2,1,0,0,0,2,0
                    ,1,1,1,0,1,1,0,0,0,3,0,1,0
                    ,0,0,0,0,1,0,0,2),byrow = T,6,6)
show(adj_mat1)
degree <- rowSums(adj_mat1)
vertex <- seq(1,6)
show(data.frame(vertex,degree))

#Adjacency matrix2 i/p based on given directed graph
adj_mat2 <- matrix(c(0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
                     ,0,1,0,0,0,0,0,0,1,0,0,0,
                     1,0,1,0,1,0,0,0,0),byrow = T,6,6)
show(adj_mat2)
degree_in <- rowSums(adj_mat2)
degree_out <- colSums(adj_mat2)
show(data.frame(vertex,degree_in,degree_out))
