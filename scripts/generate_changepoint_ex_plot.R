rm(list = ls())
library(tidyverse)

real_thetas <- c(0.1, 0.6)
N <- 30
a = 2
b = 3
change_point <- 10
x <- c(rbinom(1:change_point, 1, real_thetas[1]),rbinom((change_point+1):N, 1, real_thetas[2]))


## Initialize all parameters

# n ~ uniform 
n <- round(N*runif(1))
# theta1 ~ beta(a,b)
theta1 <- rbeta(1, a, b)
# theta2 ~ beta(a,b)
theta2 <- rbeta(1, a, b)

ds <- data.frame(outcome=x, 
                 flip=1:N, 
                 outcome_label=ifelse(x==1, 'H', 'T'),
                 coin=c(rep(1,change_point),rep(2, N-change_point)),
                 theta = c(rep(real_thetas[1],change_point), 
                           rep(real_thetas[2], N-change_point)))

ggplot(ds, aes(x = flip,
               y = theta, 
               label = outcome_label)) + 
  geom_text(color =c(rep('#1520c1', change_point),
                     rep('#c11520', N-change_point)),
            size = 4, 
            nudge_y = 0.1) + 
  geom_linerange(aes(x=flip,
                     y=NULL, ymax=theta,
                     ymin=0)) + 
  annotate("text", x = floor(change_point/2),
           y = real_thetas[1] + 0.2, 
           color="#1520c1",
           label = "Coin 1") +
  annotate("text", x = change_point + floor((N-change_point)/2),
           y = real_thetas[2] + 0.2, 
           color = '#c11520',
           label = "Coin 2") +
  scale_y_continuous(limits=c(0,1)) +
  labs(y='Coin \U03B8', x="Flip Number") +
  theme(plot.title = element_text(hjust = 0.5)) + theme_minimal()
