#Vectors q6
set.seed(50)
xVec <- sample(0:999, 250, replace = T)
yVec <- sample(0:999, 250, replace = T)

part_a <- 1:249
for (i in 1:(length(xVec)-1)) {
  part_a[i] <- yVec[i+1]-xVec[i] 
}
part_a

part_b <- 1:249
for (i in 1:(length(xVec)-1)) {
  part_b[i] <- sin(yVec[i])/cos(xVec[i+1]) 
}
part_b

part_c <- 1:248
for (i in 1:(length(xVec)-2)) {
  part_c[i] <- xVec[i] + 2*xVec[i+1] - xVec[i+2] 
}
part_c

part_d <- 1:249
for (i in 1:249) {
  part_d[i] <- (exp(-xVec[i+1]))/(xVec[i]+10)
}
part_d


#Matrices q1
A <- matrix(c(1,5,-2,1,2,-1,3,6,-3),3,3)
A %*% A %*% A

A[,3] <- A[,1]+A[,2]

#Matrices q3
matE <- matrix(rep(0,36),6,6)
matE[ abs(col(matE)-row(matE))==1 ] <- 1

#Matrices q4
Temp <- outer(0:4,0:4,FUN = "+")

#Matrices q5
matrix_a <- outer(0:4,0:4, FUN = "+")%%5
matrix_b <- outer(0:9,0:9, FUN = "+")%%10
matrix_c <- outer(0:8,9:1, FUN = "+")%%9


#simple functions q1
tmpFn1 <- function(xVec) {
  answer <- rep(0,length(xVec))
  for (i in 1:length(xVec)) {
    answer[i] <- xVec[i]^i
  }
  answer
}

tmpFn2 <- function(xVec) {
  answer2 <- rep(0,length(xVec))
  for (i in 1:length(xVec)) {
    answer2[i] <- (xVec[i]^i)/i
  }
  answer2
}

tmpFn3 <- function(x,n) {
  sum(1,tmpFn2(rep(x,n)))
}



