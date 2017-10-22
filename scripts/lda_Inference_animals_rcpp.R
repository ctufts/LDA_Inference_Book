rm(list = ls())
library(MCMCpack)
library(tidyverse)
library(Rcpp)
library(knitr)

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
shared <- 2
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


##### Plot Phi

# phi_ds <- cbind(as.tibble(t(phi)), vocab)
# emoji_text=element_text(family="OpenSansEmoji", size=20)
# ggplot(phi_ds, aes(x = vocab, y = sea)) + geom_point() + theme(axis.text.x = emoji_text )
####




xi <- 100 # average document length 
N <- rpois(M, xi) #words in each document


row_index <- 1

get_word <- function(theta, phi){
  topic <- which(rmultinom(1,1,theta)==1)
  # sample word from topic
  new_word <- which(rmultinom(1,1,phi[topic, ])==1)
  return(c(new_word, topic))  
}


theta_samples <- rdirichlet(M, alphas)
thetas <- theta_samples[rep(1:nrow(theta_samples), times = N), ]
new_words <- t(apply(thetas, 1, function(x) get_word(x,phi)))


ds <-tibble(doc_id = rep(1:length(N), times = N), 
            word   = new_words[,1],
            topic  = new_words[,2], 
            theta_a = thetas[,1],
            theta_b = thetas[,2],
            theta_c = thetas[,3]
) 





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
n_doc_topic_count <- matrix(0, nrow = M, ncol = k)
# document_topic_sum
n_doc_topic_sum  <- rep(0,M)
# topic_term_count
n_topic_term_count <- matrix(0, nrow = k, ncol = t)
# colnames(n_topic_term_count) <- unique(current_state$word)
# topic_term_sum
n_topic_sum  <- rep(0,k)
p <- rep(0, k)
# initialize topics

current_state$topic <- replicate(nrow(current_state),get_topic(k))

n_doc_topic_count <- current_state %>% group_by(doc_id, topic) %>%
  summarise(
    count = n()
  ) %>% spread(key = topic, value = count) %>% as.matrix()

n_topic_sum <- current_state %>% group_by(topic) %>%
  summarise(
    count = n()
  )  %>% select(count) %>% as.matrix() %>% as.vector()

n_topic_term_count <- current_state %>% group_by(topic, word) %>% 
  summarise(
    count = n()
  ) %>% spread(word, count) %>% as.matrix()



# topics mixtures keep converging to 1 an 0, this is incorrect,
# or it is a result fo the topics (word distributions) being too similar? 
# gibbs

# need to remove indexing by word, need numbers...

# need to read about scope...


cppFunction(
  'List gibbsLda(  NumericVector topic, NumericVector doc_id, NumericVector word,
                   NumericMatrix n_doc_topic_count,NumericMatrix n_topic_term_count,
                   NumericVector n_topic_sum, NumericVector n_doc_word_count){
  
  int alpha = 1;
  int beta  = 1;
  int cs_topic,cs_doc, cs_word, new_topic;
  int n_topics = max(topic)+1;
  int vocab_length = n_topic_term_count.ncol();
  double p_sum = 0,num_doc, denom_doc, denom_term, num_term;
  NumericVector p_new(n_topics);
  IntegerVector topic_sample(n_topics);

  for (int iter  = 0; iter < 100; iter++){
    for (int j = 0; j < word.size(); ++j){
      // change values outside of function to prevent confusion
      cs_topic = topic[j];
      cs_doc   = doc_id[j];
      cs_word  = word[j];

      // decrement counts      
      n_doc_topic_count(cs_doc,cs_topic) = n_doc_topic_count(cs_doc,cs_topic) - 1;
      n_topic_term_count(cs_topic , cs_word) = n_topic_term_count(cs_topic , cs_word) - 1;
      n_topic_sum[cs_topic] = n_topic_sum[cs_topic] -1;
    
      // get probability for each topic, select topic with highest prob
      for(int tpc = 0; tpc < n_topics; tpc++){

        // word cs_word topic tpc + beta
        num_term   = n_topic_term_count(tpc, cs_word) + beta;
        // sum of all word counts w/ topic tpc + vocab length*beta
        denom_term = n_topic_sum[tpc] + vocab_length*beta;

        
        // count of topic tpc in cs_doc + alpha
        num_doc    = n_doc_topic_count(cs_doc,tpc) + alpha;
        // total word count in cs_doc + n_topics*alpha
        denom_doc = n_doc_word_count[cs_doc] + n_topics*alpha;
      
        p_new[tpc] = (num_term/denom_term) * (num_doc/denom_doc);
        
      }
      // normalize the posteriors
      p_sum = std::accumulate(p_new.begin(), p_new.end(), 0.0);
      for(int tpc = 0; tpc < n_topics; tpc++){
        p_new[tpc] = p_new[tpc]/p_sum;
      }
      // sample new topic based on the posterior distribution
      R::rmultinom(1, p_new.begin(), n_topics, topic_sample.begin());
      
      for(int tpc = 0; tpc < n_topics; tpc++){
          if(topic_sample[tpc]==1){
            new_topic = tpc;
          }
      }
      
      // print(new_topic)
      // update counts
      n_doc_topic_count(cs_doc,new_topic) = n_doc_topic_count(cs_doc,new_topic) + 1;
      n_topic_term_count(new_topic , cs_word) = n_topic_term_count(new_topic , cs_word) + 1;
      n_topic_sum[new_topic] = n_topic_sum[new_topic] + 1;


      
  
      
      
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
lda_counts <- gibbsLda( current_state$topic-1 , current_state$doc_id-1, current_state$word-1,
           n_doc_topic_count[,-1], n_topic_term_count[,-1], n_topic_sum, N)


# calculate phi and theta's

# phi - row apply to lda_counts[[1]]

# rewrite this function and normalize by row so that they sum to 1
phi_est <- apply(lda_counts[[1]], 1, function(x) (x + beta)/(sum(x)+length(vocab)*beta) )
rownames(phi_est) <- vocab
colnames(phi) <- vocab
kable(round(phi_est, 2))
kable(t(round(phi,2)))

# theta
theta_est <- apply(lda_counts[[2]],2, function(x)(x+alphas[1])/(sum(x) + k*alphas[1]))
theta_est <- t(apply(theta_est, 1, function(x) x/sum(x)))

# figure out a way to visualize the theta distributions - probably have to first match them 
# somehow
print(head(theta_est))
(head(theta_samples))
