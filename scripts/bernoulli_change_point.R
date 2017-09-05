# gibbs sampling:

########## Try changing priors to see how it effects convergence ###############
rm(list = ls())
library(ggplot2)
real_thetas <- c(0.2, 0.6)
N <- 300
a = 2
b = 3
change_point <- 100
x <- c(rbinom(1:change_point, 1, real_thetas[1]),rbinom((change_point+1):N, 1, real_thetas[2]))


## Initialize all parameters

# n ~ uniform 
n <- round(N*runif(1))
# theta1 ~ beta(a,b)
theta1 <- rbeta(1, a, b)
# theta2 ~ beta(a,b)
theta2 <- rbeta(1, a, b)



niters = 10000
burnin = 2000
# sigma = np.diag([0.2,0.2])

# thetas = np.zeros((niters-burnin,2), np.float)
params = matrix(0, nrow = (niters-burnin), ncol=3)
for (i in 1:niters){
  
  
  z1 <- sum(x[1:n])
  if(n == N){
    z2 <- 0
  }else{
    z2 <- sum(x[(n+1):N])
  }
  theta1 = rbeta(n = 1, shape1 = a + z1, shape2 = b + n - z1)
  # get value theta2| all other vars
  theta2 = rbeta(n = 1, shape1 = a + z2, shape2 =N-n-1-z2+b)
  
  
  ## 2 things: 1 - should I be summing all the values over these? 
  # No - the product is being calculated due to the sum - should be fine
  n_multi <- rep(0, N)
  for(steps in 1:N){
    if(steps==N || theta2 == 1){
      n_multi[steps] <- log(theta1^sum(x[1:steps]) * (1-theta1)^(steps-sum(x[1:steps])))
    }else{
      n_multi[steps] <- log(theta1^sum(x[1:steps]) * (1-theta1)^(steps-sum(x[1:steps]))) +
        log(theta2^sum(x[(steps + 1):N]) * (1-theta2)^(N-steps-1-sum(x[(steps+1):N])))
    }
  }
  
  n_multi <- exp(n_multi - max(n_multi))
  # what about for n? 
  n <- which(rmultinom(1, 1, n_multi/sum(n_multi))[,1] ==1)
  if(i %% 100 == 0){
    print(paste('iteration',i ,'thetas: ', c(theta1, theta2), 'n:', n))  
  }
  if (i >= burnin){
    params[(i-burnin), ] = c(theta1,theta2, n)
  }
}

hist(params[, 1])
hist(params[, 2])
hist(params[, 3])

ds <- data.frame(x = x, theta = c(rep(real_thetas[1],N-change_point),
                                  rep(real_thetas[2],change_point)), 
                 sample_index = seq(1:length(x)))

ggplot(ds, aes(x = sample_index, y = x, color = as.factor(theta))) + geom_point()

params_df <- as.data.frame(params)
names(params_df) <- c('theta1', 'theta2', 'change_point')
ggplot(params_df, aes(x = theta2)) + 
  geom_histogram(fill="#7A99AC", color ='#1A384A' ) + 
  theme_minimal() + 
  geom_vline(xintercept = mean(params_df$theta2), color='#b7091a') +
  labs(title = expression(theta[2]~Estimate), x=expression(theta[2]), y = 'Density') 

ggplot(params_df, aes(x = theta1)) + 
  geom_histogram(fill="#7A99AC", color ='#1A384A' ) + 
  theme_minimal() + 
  geom_vline(xintercept = mean(params_df$theta1), color='#b7091a') +
  labs(title = expression(theta[1]~Estimate), x=expression(theta[1]), y = 'Density') 

ggplot(params_df, aes(x = change_point)) + 
  geom_histogram(fill="#7A99AC", color ='#1A384A') +
  theme_minimal() + 
  geom_vline(xintercept = mean(params_df$change_point), color='#b7091a') +
  labs(title = 'Change Point Estimate', x='Change Point', y = 'Density') 



