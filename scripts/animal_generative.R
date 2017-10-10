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

# 3 topics - land sea & air
# birds and amphibious have cross over
# fish - sea 100
# land animals 100 land

k <- 3
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
M <- 5 # let's create 10 documents

betas <- rep(1,length(vocab)) # dirichlet parameters for topic word distributions

xi <- 20 # lambda parameter for poisson distribution
alphas <- rep(1,k) # topic document dirichlet parameters


# theta <- matrix(rdirichlet(M, rep(1,3)), nrow = M, ncol = k)
theta <- matrix(c(c(0,0,1),
                c(0,1,0),
                c(1,0,0),
                rep(1/3, 3),
                c(0,0.5,0.5)), nrow = M, ncol = k, byrow = T)

# N <- rep(0, M)

# thetas - document topic proportion ~ Dir(alpha)
# Nm - lenght of document m ~ Poisson(xi)
# 

# calculate topic word distributions


phi <- matrix(c(land_phi, sea_phi, air_phi), nrow = k, ncol = length(vocab), 
              byrow = TRUE, dimnames = list(c('land', 'sea', 'air')))

N <- 10 #words in each document
ds <-tibble(doc_id = rep(0, N*M),
            word = rep('', N*M),
            topic = rep(0, N*M)
) 

row_index <- 1
for(m in 1:M){
  # sample topic mixture proportion for document
  # sample document length
  # N[m] <- rpois(1, xi)
  
  for(n in 1:N){
    # sample topic index , i.e. select topic
    topic <- which(rmultinom(1,1,theta[m, ])==1)
    # sample word from topic
    new_word <- vocab[which(rmultinom(1,1,phi[topic, ])==1)]
    ds[row_index,] <- c(m,new_word, topic)
    row_index <- row_index + 1
  }
}

ds %>% group_by(doc_id) %>% 
  print(paste(.$word, collapse = ' '))
