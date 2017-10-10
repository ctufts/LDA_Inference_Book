rm(list = ls())
library(MCMCpack)
library(tidyverse)


get_topic <- function(k){ 
  which(rmultinom(1,size = 1,rep(1/k,k))[,1] == 1)
} 


k <- 2 # number of topics
M <- 10 # let's create 10 documents
vocab <- c('\U1F4D8', '\U1F4D5', '\U1F4D7')
alphas <- rep(1,k) # topic document dirichlet parameters
beta <- 1


phi <- matrix(c(0.1, 0, 0.9,
                0.4, 0.4, 0.2), 
              nrow = k, 
              ncol = length(vocab), 
              byrow = TRUE)


xi <- 100 # average document length 
N <- rpois(M, xi) #words in each document
ds <-tibble(doc_id = rep(0,sum(N)), 
            word   = rep('', sum(N)),
            topic  = rep(0, sum(N)), 
            theta_a = rep(0, sum(N)),
            theta_b = rep(0, sum(N))
) 

row_index <- 1
for(m in 1:M){
  theta <-  rdirichlet(1, alphas)
  
  for(n in 1:N[m]){
    # sample topic index , i.e. select topic
    topic <- which(rmultinom(1,1,theta)==1)
    # sample word from topic
    new_word <- vocab[which(rmultinom(1,1,phi[topic, ])==1)]
    ds[row_index,] <- c(m,new_word, topic,theta)
    row_index <- row_index + 1
  }
}

ds$doc_id <- as.numeric(ds$doc_id)



ds %>% group_by(doc_id) %>% summarise(
  tokens = paste(word, collapse = ' '), 
  topic_a = round(as.numeric(unique(theta_a)), 2), 
  topic_b = round(as.numeric(unique(theta_b)), 2) 
) %>% kable()


######### Inference ############### 



current_state <- ds %>% dplyr::select(doc_id, word, topic)
current_state$topic <- NA

# clean up the names of some of these variables so it is easier to follow the 
# general pseudo code


t <- length(unique(current_state$word))

# n_doc_topic_count  
n_doc_topic_count <- matrix(0, nrow = m, ncol = k)
# document_topic_sum
n_doc_topic_sum  <- rep(0,m)
# topic_term_count
n_topic_term_count <- matrix(0, nrow = k, ncol = t)
colnames(n_topic_term_count) <- unique(current_state$word)
# topic_term_sum
n_topic_sum  <- rep(0,k)
p <- rep(0, k)
# initialize topics
for( i in 1:nrow(current_state)){
  current_state$topic[i] <- get_topic(k)
  n_doc_topic_count[current_state$doc_id[i],current_state$topic[i]] <- n_doc_topic_count[current_state$doc_id[i],current_state$topic[i]] + 1
  n_doc_topic_sum[current_state$doc_id[i]] <- n_doc_topic_sum[current_state$doc_id[i]] + 1
  n_topic_term_count[current_state$topic[i] , current_state$word[i]] <- n_topic_term_count[current_state$topic[i] ,
                                                               current_state$word[i]] + 1
  n_topic_sum[current_state$topic[i]] = n_topic_sum[current_state$topic[i]] + 1
  
}



# topics mixtures keep converging to 1 an 0, this is incorrect,
# or it is a result fo the topics (word distributions) being too similar? 
# gibbs
burnin <- 500
for (iter in 1:100){
  # for(i in 1:m){
  # cs <- filter(current_state, doc_id == m)
  # either fix the indexing or need to update 2's?
  
  for(j in 1:nrow(current_state)){
    # decrement counts
    cs_topic <- current_state$topic[j]
    cs_doc   <- current_state$doc_id[j]
    cs_word  <- current_state$word[j]
    
    n_doc_topic_count[cs_doc,cs_topic] <- n_doc_topic_count[cs_doc,cs_topic] - 1
    n_doc_topic_sum[cs_doc] <- n_doc_topic_sum[cs_doc] - 1
    n_topic_term_count[cs_topic , cs_word] <- n_topic_term_count[cs_topic , cs_word] - 1
    n_topic_sum[cs_topic] = n_topic_sum[cs_topic] -1
    # get probability for each topic, select topic with highest prob
    # print(n_doc_topic_count)
    
    for(topic in 1:k){
      p[topic] <- (n_topic_term_count[topic, cs_word] + beta) * (n_doc_topic_count[cs_doc,topic] + alphas[k])/
        sum(n_topic_term_count[topic,] + beta)
      # print(p[topic])
    }
    new_topic <- which.max(p)
    # print(new_topic)
    # update counts
    n_doc_topic_count[cs_doc,new_topic] <- n_doc_topic_count[cs_doc,new_topic] + 1
    n_doc_topic_sum[cs_doc] <- n_doc_topic_sum[cs_doc] + 1
    n_topic_term_count[new_topic , cs_word] <- n_topic_term_count[new_topic , cs_word] + 1
    n_topic_sum[new_topic] = n_topic_sum[new_topic] + 1

    
    # update current_state
    current_state$topic[j] <- new_topic
    
  }
 
}



# calculate the distributions of words (topic) and topic/doc_id

# estimated vs. real distributions used to create the docs

current_state %>% group_by(doc_id) %>%
  summarise(topic_a = sum(topic ==1)/n(), 
            topic_b = sum(topic ==2)/n())

ds %>% select(doc_id, theta_a, theta_b) %>% distinct()


# word distributions of topics

current_state %>% group_by(word) %>%
  summarise(
    topic_a = sum(topic == 1)/sum(current_state$topic == 1),
    topic_b = sum(topic == 2)/sum(current_state$topic == 2)
  )

