rm(list = ls())
library(tidyverse)
library(dplyr)

# n_docs <- 4
# N <- 4 # words

get_topic <- function(k){
  which(rmultinom(1,size = 1,rep(1/k,k))[,1] == 1)
} 

create_documents <- function(k, M, phi, vocab){
  # k - topics
  # M - documents
  # phi - word distribution for each topic
  # vocab - vocabulary
  betas <- rep(1,length(vocab)) # dirichlet parameters for topic word distributions
  
  xi <- 20 # lambda parameter for poisson distribution
  alphas <- rep(1,k) # topic document dirichlet parameters
  
  topic_1 <- runif(M)
  theta <- matrix(c(topic_1, 1-topic_1), nrow = M, ncol = k)
  
  N <- rep(0, M)
  
  
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
  ds
}

k <- 2 # number of topics
m <- 10 # let's create 10 documents
beta <- 1
# k <- 2
alpha <- 5
p <- rep(0,k)
vocab <- c('\U1F4D8', '\U1F4D5', '\U1F4D7')
phi <- matrix(c(0.1, 0, 0.9, 0.4, 0.4, 0.2), nrow = k, ncol = length(vocab), 
              byrow = TRUE)
colnames(phi) <- vocab


docs_origin <- create_documents(k,m, phi, vocab)
current_state <- docs_origin[,c('document', 'word', 'topic')]
current_state$topic <- NA

# clean up the names of some of these variables so it is easier to follow the 
# general pseudo code


t <- length(unique(current_state$word))

# n_mk  doc_topic_count
n_mk <- matrix(0, nrow = m, ncol = k)
# document_topic_sum
n_m  <- rep(0,m)
# topic_term_count
n_kt <- matrix(0, nrow = k, ncol = t)
colnames(n_kt) <- unique(current_state$word)
# topic_term_sum
n_k  <- rep(0,k)

# initialize topics
for( i in 1:nrow(current_state)){
  current_state$topic[i] <- get_topic(k)
  n_mk[current_state$document[i],current_state$topic[i]] <- n_mk[current_state$document[i],current_state$topic[i]] + 1
  n_m[current_state$document[i]] <- n_m[current_state$document[i]] + 1
  n_kt[current_state$topic[i] , current_state$word[i]] <- n_kt[current_state$topic[i] ,
                                                               current_state$word[i]] + 1
  n_k[current_state$topic[i]] = n_k[current_state$topic[i]] + 1
  
}




# gibbs

for (iter in 1:1000){
  # for(i in 1:m){
  # cs <- filter(current_state, document == m)
  # either fix the indexing or need to update 2's?
  
  for(j in 1:nrow(current_state)){
    # decrement counts
    cs_topic <- current_state$topic[j]
    cs_doc   <- current_state$document[j]
    cs_word  <- current_state$word[j]
    
    n_mk[cs_doc,cs_topic] <- n_mk[cs_doc,cs_topic] - 1
    n_m[cs_doc] <- n_m[cs_doc] - 1
    n_kt[cs_topic , cs_word] <- n_kt[cs_topic , cs_word] - 1
    n_k[cs_topic] = n_k[cs_topic] -1
    # get probability for each topic, select topic with highest prob
    # print(n_mk)
    
    for(topic in 1:k){
      p[topic] <- (n_kt[topic, cs_word] + beta) * (n_mk[cs_doc,topic] + alpha)/
        sum(n_kt[topic,] + beta)
      # print(p[topic])
    }
    new_topic <- which.max(p)
    # print(new_topic)
    # update counts
    n_mk[cs_doc,new_topic] <- n_mk[cs_doc,new_topic] + 1
    n_m[cs_doc] <- n_m[cs_doc] + 1
    n_kt[new_topic , cs_word] <- n_kt[new_topic , cs_word] + 1
    n_k[new_topic] = n_k[new_topic] + 1
    # print(n_mk)
    
    # update current_state
    current_state$topic[j] <- new_topic
  }
  # }
}



# calculate the distributions of words (topic) and topic/document

# estimated vs. real distributions used to create the docs

current_state %>% group_by(document) %>%
  summarise(topic_a = sum(topic ==1)/n(), 
            topic_b = sum(topic ==2)/n())

docs_origin %>% select(document, prop_a, prop_b) %>% distinct()


# word distributions of topics

current_state %>% group_by(word) %>%
  summarise(
    topic_a = sum(topic == 1)/sum(current_state$topic == 1),
    topic_b = sum(topic == 2)/sum(current_state$topic == 2)
  )

