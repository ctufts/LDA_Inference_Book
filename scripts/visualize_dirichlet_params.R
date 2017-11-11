rm(list = ls())
library(MCMCpack)
library(tidyverse)



############### Alphas = 100 ###################
alpha <- c(100,100,100)
trials <- 10000
x <- rdirichlet(trials, alpha)
colnames(x) <- c('theta_1', 'theta_2', 'theta_3')
ds <- cbind(as.tibble(x), trial = 1:trials) %>% 
  gather(theta, word, -trial)

# ggplot(ds, aes(color = theta, x = word)) + geom_line(stat='density') + 
#   theme_minimal()

ggplot(ds, aes(color = theta, fill = theta, x = theta, y = word)) + geom_boxplot(alpha = 0.3) + 
  theme_minimal() + 
  labs(y='\U03B8', x = '', title = paste0("\U03B1 = ",unique(alpha)) ) + 
  scale_x_discrete(labels = c(expression("\U03B1"[1]),
                              expression("\U03B1"[2]),
                              expression("\U03B1"[3]))) + 
  scale_fill_discrete(guide = FALSE) + 
  scale_color_discrete(guide = FALSE)+ 
  scale_y_continuous(limits = c(0,1))



################## Alphas = 1 #####################
alpha <- c(1,1,1)
  x <- rdirichlet(trials, alpha)
colnames(x) <- c('theta_1', 'theta_2', 'theta_3')
ds <- cbind(as.tibble(x), trial = 1:trials) %>% 
  gather(theta, word, -trial)

ggplot(ds, aes(color = theta, fill = theta, x = theta, y = word)) + geom_boxplot(alpha = 0.3) + 
  theme_minimal() + 
  labs(y='\U03B8', x = '', title = paste0("\U03B1 = ",unique(alpha)) ) + 
  scale_x_discrete(labels = c(expression("\U03B1"[1]),
                              expression("\U03B1"[2]),
                              expression("\U03B1"[3]))) + 
  scale_fill_discrete(guide = FALSE) + 
  scale_color_discrete(guide = FALSE) + 
  scale_y_continuous(limits = c(0,1))



# ggplot(ds, aes(color = theta, x = word)) + geom_line(stat='density') + 
#   theme_minimal()
# 
# ggplot(ds, aes(color = theta, x = theta, y = word)) + geom_boxplot() + 
#   theme_minimal()


############### Alphas = 10 ####################

alpha <- c(10,10,10)
x <- rdirichlet(trials, alpha)
colnames(x) <- c('theta_1', 'theta_2', 'theta_3')
ds <- cbind(as.tibble(x), trial = 1:trials) %>% 
  gather(theta, word, -trial)

ggplot(ds, aes(color = theta, fill = theta, x = theta, y = word)) + geom_boxplot(alpha = 0.3) + 
  theme_minimal() + 
  labs(y='\U03B8', x = '', title = paste0("\U03B1 = ",unique(alpha)) ) + 
  scale_x_discrete(labels = c(expression("\U03B1"[1]),
                              expression("\U03B1"[2]),
                              expression("\U03B1"[3]))) + 
  scale_fill_discrete(guide = FALSE) + 
  scale_color_discrete(guide = FALSE)+ 
  scale_y_continuous(limits = c(0,1))



######### different alphas - low #################
alpha <- c(10,50,20)
alpha_prop <- alpha/sum(alpha)
x <- rdirichlet(trials, alpha)
colnames(x) <- c('theta_1', 'theta_2', 'theta_3')
ds <- cbind(as.tibble(x), trial = 1:trials) %>% 
  gather(theta, word, -trial)

ggplot(ds, aes(color = theta, fill=theta, x = word)) + geom_histogram(position='identity', alpha = 0.1) +
  # geom_line(stat='density') + 
  theme_minimal() + 
  labs(x = "\U03B8", y = "Count") + 
  scale_color_discrete(label = alpha,
                       name = "\U03B1" ) +
  scale_fill_discrete(label = alpha,
                       name = "\U03B1" )


######### different alphas - high #################
alpha <- c(100,500,200)
alpha_prop <- alpha/sum(alpha)
x <- rdirichlet(trials, alpha)
colnames(x) <- c('theta_1', 'theta_2', 'theta_3')
ds <- cbind(as.tibble(x), trial = 1:trials) %>% 
  gather(theta, word, -trial)

ggplot(ds, aes(color = theta, fill=theta, x = word)) + geom_histogram(position='identity', alpha = 0.1) +
  # geom_line(stat='density') + 
  theme_minimal() + 
  labs(x = "\U03B8", y = "Count") + 
  scale_color_discrete(label = alpha,
                       name = "\U03B1" ) +
  scale_fill_discrete(label = alpha,
                      name = "\U03B1" )



