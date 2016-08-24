#q6
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




