rm(list = ls())
library(MCMCpack)
library(tidyverse)
library(Rcpp)


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
M <- 1000 # let's create 10 documents

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


row_index <- 1

get_word <- function(theta, phi){
  topic <- which(rmultinom(1,1,theta)==1)
  # sample word from topic
  new_word <- which(rmultinom(1,1,phi[topic, ])==1)
  return(c(new_word, topic))  
}


thetas <- rdirichlet(M, alphas)
thetas <- thetas[rep(1:nrow(thetas), times = N), ]
new_words <- t(apply(thetas, 1, function(x) get_word(x,phi)))


ds <-tibble(doc_id = rep(0,sum(N)), 
            word   = rep(0, sum(N)),
            topic  = rep(0, sum(N)), 
            theta_a = rep(0, sum(N)),
            theta_b = rep(0, sum(N)),
            theta_c = rep(0, sum(N))
) 

# topics <- apply(t(rmultinom(10,1,theta)), 1, function(x)which(x==1))

for(m in 1:M){
  theta <-  rdirichlet(1, alphas)
  
  for(n in 1:N[m]){
    # sample topic index , i.e. select topic
    topic <- which(rmultinom(1,1,theta)==1)
    # sample word from topic
    # new_word <- vocab[which(rmultinom(1,1,phi[topic, ])==1)]
    new_word <- which(rmultinom(1,1,phi[topic, ])==1)
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
# colnames(n_topic_term_count) <- unique(current_state$word)
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

# need to remove indexing by word, need numbers...

# need to read about scope...


cppFunction(
  'List gibbsLda(  NumericVector topic, NumericVector doc_id, NumericVector word,
                   NumericMatrix n_doc_topic_count,NumericMatrix n_topic_term_count,
                   NumericVector n_topic_sum){
  
  int alpha = 1;
  int beta  = 1;
  int cs_topic = 0;
  int cs_doc   = 0;
  int cs_word  = 0;
  int new_topic = 0;
  int n_topics = max(topic)+1;
  double p = 0;
  double p_new = 0;
  //NumericVector n_topic_sum(topic.size());

  for (int iter  = 0; iter < 5000; iter++){
    for (int j = 0; j < topic.size(); ++j){
      //Rcout << j << std::endl;
      // change values outside of function to prevent confusion
      cs_topic = topic[j];
      cs_doc   = doc_id[j];
      cs_word  = word[j];

      // decrement counts      
      n_doc_topic_count(cs_doc,cs_topic) = n_doc_topic_count(cs_doc,cs_topic) - 1;
      n_topic_term_count(cs_topic , cs_word) = n_topic_term_count(cs_topic , cs_word) - 1;
      n_topic_sum[cs_topic] = n_topic_sum[cs_topic] -1;
    
      // get probability for each topic, select topic with highest prob
      new_topic = 0;
      p = 0;
      for(int probs = 0; probs < n_topics; probs++){
        p_new = (n_topic_term_count(probs, cs_word) + beta) * (n_doc_topic_count(cs_doc,probs) + alpha)/((n_topic_sum[probs]) + beta);
      
        if(p_new > p){
          // offset for indexing in R
          // Rcout << "The value is " << p_new << std::endl;
          new_topic = probs;
          p = p_new;
        }
      }
      
      
      // print(new_topic)
      // update counts
      n_doc_topic_count(cs_doc,new_topic) = n_doc_topic_count(cs_doc,new_topic) + 1;
      n_topic_term_count(new_topic , cs_word) = n_topic_term_count(new_topic , cs_word) + 1;
      n_topic_sum[new_topic] = n_topic_sum[new_topic] + 1;


      
  
      // Rcout << "The value is " << new_topic << std::endl;
      
      
      // update current_state
      topic[j] = new_topic;
      
    }
    
    }
return List::create(
  n_topic_term_count,
  n_doc_topic_count);
}
')



# minus 1 in, add 1 out
gibbsLda( current_state$topic-1 , current_state$doc_id-1, current_state$word-1,
           n_doc_topic_count, n_topic_term_count, n_topic_sum)

