rm(list = ls())
library(MCMCpack)
library(tidyverse)
library(Rcpp)
library(knitr)

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