# source: https://stats.stackexchange.com/questions/15198/dirichlet-distribution-plot-in-r

require(MCMCpack)
alpha <- c(1,5,10)
draws <- 15
dimen <- 3
x <- rdirichlet(draws, alpha)

dat <- data.frame(item=factor(rep(1:dimen,draws)), 
                  draw=factor(rep(1:draws,each=dimen)), 
                  value=as.vector(t(x)))

library(ggplot2)
ggplot(dat,aes(x=item,y=value,ymin=0,ymax=value)) + 
  geom_point(colour=I("blue"))       + 
  geom_linerange(colour=I("blue"))   + 
  facet_wrap(~draw,ncol=5)           + 
  scale_y_continuous(lim=c(0,1))     +
  theme(panel.border = element_rect(fill=0, colour="black"))



###### Dirichlet with k = 2 same as beta distribution:
x_matrix<- cbind(seq(0,1, 0.01), seq(1, 0, -0.01))
plot(x_matrix[,1],ddirichlet(x_matrix, c(1,1)))
plot(x_matrix[,1],ddirichlet(x_matrix, c(10,10)))
