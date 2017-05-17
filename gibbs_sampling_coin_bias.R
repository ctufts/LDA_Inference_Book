rm(list = ls())
library(ggplot2)
# gibbs sampling:

a = 2
b = 2

z1 = 11
N1 = 14
z2 = 7
N2 = 14


theta = rep(0.5,2)
niters = 10000
burnin = 500

thetas = matrix(0, nrow = (niters-burnin), ncol=2)
for (i in 1:niters){

  theta1 = rbeta(n = 1, shape1 = a + z1, shape2 = b + N1 - z1)
  # get value theta2| all other vars
  theta2 = rbeta(n = 1, shape1 = a + z2, shape2 =b + N2 - z2)
  
  if (i >= burnin){
    thetas[(i-burnin), ] = c(theta1, theta2)
  }
}


ds <- data.frame(theta1 = thetas[,1], theta2= thetas[,2])
ggplot(ds, aes(x=theta1)) + geom_histogram(color='#1303a5', fill='#200cd1') + 
  labs(title = 'Theta 1 Estimate') + geom_vline(xintercept = mean(ds$theta1), color='#b7091a')

ggplot(ds, aes(x=theta2)) + geom_histogram(color='#1303a5', fill='#200cd1') + 
  labs(title = 'Theta 2 Estimate') + geom_vline(xintercept = mean(ds$theta2), color='#b7091a')

