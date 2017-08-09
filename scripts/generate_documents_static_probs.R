# Description:
# Generate Documents
# Knowns: # of topics
# Unknown: document length
#          word distribution
#          topic distribution
#
# Figure 7 in Heinrich
#

rm(list = ls())
library(MCMCpack)
library(tidyverse)


k <- 2 # number of topics
M <- 10 # let's create 10 documents
vocab <- c('\U1F4D8', '\U1F4D5', '\U1F4D7')
betas <- rep(1,length(vocab)) # dirichlet parameters for topic word distributions

xi <- 20 # lambda parameter for poisson distribution
alphas <- rep(1,k) # topic document dirichlet parameters

topic_1 <- runif(M)
theta <- matrix(c(topic_1, 1-topic_1), nrow = M, ncol = k)

N <- rep(0, M)

# thetas - document topic proportion ~ Dir(alpha)
# Nm - lenght of document m ~ Poisson(xi)
# 

# calculate topic word distributions


phi <- matrix(c(0.1, 0, 0.9, 0.4, 0.4, 0.2), nrow = k, ncol = length(vocab), 
              byrow = TRUE)


ds <-NULL
for(m in 1:M){
  # sample topic mixture proportion for document
  # sample document length
  N[m] <- rpois(1, xi)
  
  for(n in 1:N[m]){
    # sample topic index , i.e. select topic
    topic <- which(rmultinom(1,1,theta[m, ])==1)
    # sample word from topic
    new_word <- vocab[which(rmultinom(1,1,phi[topic, ])==1)]
    ds <- rbind(ds, tibble(word = new_word, topic = topic, document = m,
                           prop_a = theta[m, 1], prop_b = theta[m,2], 
                           n = n))
  }
}