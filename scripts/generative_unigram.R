# Description: Generate document based on one document's prior word distributions
# i.e. this is used for the dirichlet prior
# word is then generated based on the parameter provided by the dirichlet 
# random variable as input to a multinomial random variable
# This is an example of equations 53-54 in Heinrich's paper
# 




rm(list = ls())
library(MCMCpack)
library(tidyverse)



# use letters function as your vocabulary
v <- c('red', 'green', 'blue')
d1 <- c(.1, .1, .8)
d2 <- c(.1, .8, .1)
d3 <- c(.8, .1, .1)


documents <- list(d1 = rep(v, d1*100), 
                  d2 = rep(v, d2*100), 
                  d3 = rep(v, d3*100))

# generate text based only on document #1

words_d1 <- documents$d1
nwords_d1 <- 100 # lenght of new document
 #rep(1,length(unique(words_d1)))
word_counts <- table(words_d1)
alphas <-  word_counts
new_doc <- rep('', nwords_d1)

for(i in 1:nwords_d1){
  p = rdirichlet(1,alphas)
  new_doc[i] <- names(word_counts)[which(rmultinom(1, 1, p) == 1)]
}

table(new_doc)
books <- tibble(label = c('blue', 'red', 'green'), 
                    code = c('\U1F4D8', '\U1F4D5', '\U1F4D7'))


cat(sapply(new_doc, function(x) books$code[which(books$label == x)]))
# mixture of our 3 docs ....



