rm(list = ls())
library(MCMCpack)
library(tidyverse)


get_topic <- function(k){ 
  which(rmultinom(1,size = 1,rep(1/k,k))[,1] == 1)
} 


# 3 topics - land sea & air
# birds and amphibious have cross over
# fish - sea 100
# land animals 100 land

k <- 3
beta <- 1
# whale1, whale2, FISH1, FISH2,OCTO
sea_animals <- c('\U1F40B', '\U1F433','\U1F41F', '\U1F420', '\U1F419')

# crab, alligator, TURTLE,SNAKE
amphibious  <- c('\U1F980', '\U1F40A', '\U1F422', '\U1F40D')

# CHICKEN, TURKEY, DUCK, PENGUIN
birds       <- c('\U1F413','\U1F983','\U1F426','\U1F427')
# SQUIRREL, ELEPHANT, COW, RAM, CAMEL
land_animals<- c('\U1F43F','\U1F418','\U1F402','\U1F411','\U1F42A')




cat(birds)
cat(land_animals)
cat(sea_animals)
cat(amphibious)

vocab <- c(sea_animals, amphibious, birds, land_animals) 
cat(vocab)

# equal probability 1/18
# 0 - animals that are not possible
# 1 - for shared
# 4 - non-shared
shared <- 1
non_shared <- 4
not_present <- 0

land_phi <- c(rep(not_present, length(sea_animals)),
              rep(shared, length(amphibious)),
              rep(non_shared, 2), # turkey and chicken can't fly
              rep(shared, 2), # regular bird and pengiun
              rep(non_shared, length(land_animals)))
land_phi <- land_phi/sum(land_phi)


sea_phi <- c(rep(non_shared, length(sea_animals)),
             rep(shared, length(amphibious)),
             rep(not_present, 2), # turkey and chicken can't fly 
             rep(shared, 2), # regular bird and pengiun 
             rep(not_present, length(land_animals)))
sea_phi <- sea_phi/sum(sea_phi)

air_phi <- c(rep(not_present, length(sea_animals)),
             rep(not_present, length(amphibious)),
             rep(not_present, 2), # turkey and chicken can't fly 
             non_shared, # regular bird
             not_present, # penguins can't fly
             rep(not_present, length(land_animals)))
air_phi <- air_phi/sum(air_phi)



k <- 3 # number of topics
M <- 100 # let's create 10 documents

betas <- rep(1,length(vocab)) # dirichlet parameters for topic word distributions

xi <- 20 # lambda parameter for poisson distribution
alphas <- rep(1,k) # topic document dirichlet parameters




# thetas - document topic proportion ~ Dir(alpha)
# Nm - lenght of document m ~ Poisson(xi)
# 

# calculate topic word distributions


phi <- matrix(c(land_phi, sea_phi, air_phi), nrow = k, ncol = length(vocab), 
              byrow = TRUE, dimnames = list(c('land', 'sea', 'air')))

xi <- 100 # average document length 
N <- rpois(M, xi) #words in each document
ds <-tibble(doc_id = rep(0,sum(N)), 
            word   = rep('', sum(N)),
            topic  = rep(0, sum(N)), 
            theta_a = rep(0, sum(N)),
            theta_b = rep(0, sum(N)),
            theta_c = rep(0, sum(N))
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



# ds %>% group_by(doc_id) %>% summarise(
#   tokens = paste(word, collapse = ' '), 
#   topic_a = round(as.numeric(unique(theta_a)), 2), 
#   topic_b = round(as.numeric(unique(theta_b)), 2), 
#   topic_c = round(as.numeric(unique(theta_c)), 2) 
# ) %>% kable()


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
for (iter in 1:1000){
  # for(i in 1:m){
  # cs <- filter(current_state, doc_id == m)
  # either fix the indexing or need to update 2's?
  print(iter)
  for(j in 1:nrow(current_state)){
    # decrement counts
    # print(j)
    cs_topic <- current_state$topic[j]
    cs_doc   <- current_state$doc_id[j]
    cs_word  <- current_state$word[j]
    
    n_doc_topic_count[cs_doc,cs_topic] <- n_doc_topic_count[cs_doc,cs_topic] - 1
    # n_doc_topic_sum[cs_doc] <- n_doc_topic_sum[cs_doc] - 1
    n_topic_term_count[cs_topic , cs_word] <- n_topic_term_count[cs_topic , cs_word] - 1
    # n_topic_sum[cs_topic] = n_topic_sum[cs_topic] -1
    # get probability for each topic, select topic with highest prob
    # print(n_doc_topic_count)
    
    p <- (n_topic_term_count[1:k, cs_word] + beta) * (n_doc_topic_count[cs_doc,1:k] + alphas)/
        (rowSums(n_topic_term_count[1:k, ]) + beta)
      # print(p[topic])
  
    new_topic <- which.max(p)
    # print(new_topic)
    # update counts
    n_doc_topic_count[cs_doc,new_topic] <- n_doc_topic_count[cs_doc,new_topic] + 1
    # n_doc_topic_sum[cs_doc] <- n_doc_topic_sum[cs_doc] + 1
    n_topic_term_count[new_topic , cs_word] <- n_topic_term_count[new_topic , cs_word] + 1
    # n_topic_sum[new_topic] = n_topic_sum[new_topic] + 1
    
    
    # update current_state
    current_state$topic[j] <- new_topic
    
  }
  
}



# calculate the distributions of words (topic) and topic/doc_id

# estimated vs. real distributions used to create the docs

current_state %>% group_by(doc_id) %>%
  summarise(topic_a = sum(topic ==1)/n(), 
            topic_b = sum(topic ==2)/n())

ds %>% dplyr::select(doc_id, theta_a, theta_b, theta_c) %>% distinct()


# word distributions of topics

current_state %>% group_by(word) %>%
  summarise(
    topic_a = 100*sum(topic == 1)/sum(current_state$topic == 1),
    topic_b = 100*sum(topic == 2)/sum(current_state$topic == 2),
    topic_c = 100*sum(topic == 3)/sum(current_state$topic == 3)
  ) %>% arrange(topic_c) %>% kable()

