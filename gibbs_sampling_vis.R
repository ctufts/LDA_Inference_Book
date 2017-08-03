library(ggplot2)

theta1 <- c( 0, 0.2, 0.3, 0.1)
theta2 <- c( 0, 0.5, 0.1, 0.3)
ds <- NULL
for( i in 2:length(theta1)){
  ds <- rbind(ds, data.frame(theta1 = theta1[i], theta2 = theta2[(i-1)]))
  ds <- rbind(ds, data.frame(theta1 = theta1[i], theta2 = theta2[i]))
  
  g <- ggplot(ds[(i-1):i,], aes(x = theta1, y = theta2)) + geom_point() + 
    scale_x_continuous(limits = c(0,1)) +
    scale_y_continuous(limits = c(0,1)) 
  print(g)
  
}
# maybe grid expand the data? need to specify each step (i.e. fill in the blanks)
# provide data label so we can change the size or shape based on which is being calculated


ds <- data.frame(theta1, theta2)

for(i in 1:(nrow(ds)-1)){
  
}

