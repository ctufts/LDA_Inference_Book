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

topic_doc_distributions <- c(0.5, 0.5)


# thetas - document topic proportion ~ Dir(alpha)
# Nm - lenght of document m ~ Poisson(xi)
# 

# calculate topic word distributions


phi <- matrix(c(0.1, 0, 0.9, 0.4, 0.4, 0.2), nrow = k, ncol = length(vocab), 
              byrow = TRUE)


books <- tibble(label = c('blue', 'red', 'green'), 
                code = c('\U1F4D8', '\U1F4D5', '\U1F4D7'))


get_word <- function(theta, phi){
  vocab <- c('\U1F4D8', '\U1F4D5', '\U1F4D7')
  topic <- which(rmultinom(1,1,theta)==1)
  # sample word from topic
  new_word <- vocab[which(rmultinom(1,1,phi[topic, ])==1)]
  return(c(topic, new_word))  
}

selected_words <- t(replicate(100, get_word(theta= topic_doc_distributions, phi)))
df <- tibble(topic = as.numeric(selected_words[,1]),
             word_label = books$label[match(selected_words[, 2],books$code)],
             word_encodings = selected_words[, 2])