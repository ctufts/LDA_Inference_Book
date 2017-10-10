library(Rcpp)
library(microbenchmark)
cppFunction(
"NumericVector logmodc(int t, double yinit, double r,
            double k, double thetasd){
            NumericVector y(t);
            y[0] = yinit;
            Rf_PrintValue(y);
            NumericVector theta = rnorm(t, 0, thetasd);
            for (int i = 1; i < t; ++i){
            y[i] = y[i-1]*(r - r*(y[i-1] / k)) * exp(theta[i]);
            }
            return y;
            }
            ")


t <- 100
yinit <- 1
k <- 20
thetasd <- 0.1
r <- 0.2
# Now we can run our model. I am just going to plug the models straight into microbenchmark, so I can compare their times.

library(microbenchmark)
mb1 <- microbenchmark(
  logmodc(t, yinit, 1.4, k, thetasd)
  
)
mb1
