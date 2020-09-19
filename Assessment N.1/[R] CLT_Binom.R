CLT_binomial <- function(iteration, sample_size, mu, var){
  mean_X <- rep(0,iteration)
  Z <- rep(0,iteration)
  title <- sprintf("Simulation with sample size n = %s", sample_size)
  
  for(i in 1:iteration){
    # sampling binomial random variable X of outcome = 1 with probability = 0.5
    X <- rbinom(sample_size, 1, 0.5)
    mean_X[i] <- mean(X)
    Z[i] <- (mean_X[i] - mu)/(sqrt(var/sample_size))
  }
  hist(Z,
       prob = TRUE,
       xlim = c(-5, 5),
       xlab = "Binomial Random Variables Z",
       main = title,
       col = "red",
       breaks = 25,
       freq = FALSE)
  curve(dnorm(x, mean = 0, sd = 1),
        add = T,
        col = "blue")
}
par(mfrow = c(2,2))
CLT_binomial(1000, 2, 0.5, 0.25)
CLT_binomial(1000, 25, 0.5, 0.25)
CLT_binomial(1000, 100, 0.5, 0.25)
CLT_binomial(1000, 1000, 0.5, 0.25)