# Problem 2 (Central Limit Theorem)}
# Let X be a Normally distributed random variable with mean mu_X = -10 and variance \sigma^2_X = 1. 
# Write the R code to show the Central Limit Theorem in action.
# Build a Monte Carlo experiment in which you set the sample size n equal to 2, 25, 100 and 1000 and
# 1. show that the distribution of (bar(X)-mu_X)/(sqrt(sigma^2_X/n) converges to a standardized Normal distribution,
# that is to a Normal distribution with mean and variance equal to 0 and 1 respectively.
# 2. represent graphically the four cases as in slide 23 in my Class 1 presentation.

# [0]
set.seed(2)
# [1]
CLT <- function(iteration, sample_size, mu, var){
  # [2]
  mean_X <- rep(0,iteration)
  Z <- rep(0,iteration)
  title <- sprintf("Simulation with sample size n = %s", sample_size)
  # [3]
  for(i in 1:iteration){
    X <- rnorm(sample_size, mean = mu, sd = sqrt(var))
    mean_X[i] <- mean(X)
    Z[i] <- (mean_X[i] - mu)/(sqrt(var/sample_size))
  }
  # [4]
  hist(Z,
       prob = TRUE,
       xlim = c(-5, 5),
       xlab = "Normal Random Variables Z",
       main = title,
       col = "red",
       breaks = 25,
       freq = FALSE)
  # [5]
  curve(dnorm(x, mean = 0, sd = 1),
        add=T,
        col="blue")
}
par(mfrow = c(2,2))
# [6]
CLT(1000,2,-10,1)
CLT(1000,25,-10,1)
CLT(1000,100,-10,1)
CLT(1000,1000,-10,1)
