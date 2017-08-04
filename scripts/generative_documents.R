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
M <- 5 # let's create 10 documents
vocab <- c('\U1F4D8', '\U1F4D5', '\U1F4D7')
betas <- rep(1,length(vocab)) # dirichlet parameters for topic word distributions

xi <- 20 # lambda parameter for poisson distribution
alphas <- rep(1,k) # topic document dirichlet parameters

theta <- matrix(0, nrow = M, ncol = k)

N <- rep(0, M)

# thetas - document topic proportion ~ Dir(alpha)
# Nm - lenght of document m ~ Poisson(xi)
# 

# calculate topic word distributions


phi <- rdirichlet(k, betas)


ds <-NULL
for(m in 1:M){
  # sample topic mixture proportion for document
  theta[m, ] <- rdirichlet(1, alphas)
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

ggplot(ds, aes(x = factor(document), fill = factor(topic))) + 
  geom_bar(position = position_dodge(preserve = "single")) + coord_flip() + 
  scale_fill_manual(values=c("#7A99AC", "#E4002B"), name = 'Topic') + 
  labs(x = 'Document', y = '# of Words')

temp <- ds %>% group_by(document, topic) %>% 
  summarise(words = paste(word, collapse = ' '),
            topic_distribution_parameter = if_else(topic[1] == 1, prop_a[1], prop_b[1])
            )


  
  


