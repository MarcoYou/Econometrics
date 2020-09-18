# Problem 1 (Law of Large Numbers)
# Let X be a random variable representing the roll of a fair dice.
# Write the R code to show the Law of Large Numbers in action. Build a Monte Carlo experiment
# in which you set the sample size n equal to 2, 25, 100 and 1000 and
# 1. show that the range of variation of the sample average $\bar{X}$ shrinks.
# 2. represent graphically the four cases as in slide 18 in my Class 1 presentation. 

# [0]
set.seed(1)
# [1]
LLN <- function(iteration, sample_size){
  # [2]
  mean_vector <- rep(0, iteration)
  title <- sprintf("Simulation with sample size n = %s", sample_size)
  # [3]
  for(n in 1:iteration){
    mean_vector[n] <- mean(sample(1:6, size = sample_size, replace = TRUE))
  }
  # [4]
  plot(mean_vector,
       rep(0.2, iteration),
       xlab = "Distribution of Sample Averages",
       ylab = "",
       xlim = c(1,6),
       ylim = c(0.1,1),
       pch = 16,
       main = title
       )
  # [5]
  abline(v = mean(mean_vector), col = "blue")
  abline(v = 3.5, col = "red")
}
par(mfrow=c(2,2))
# [6]
LLN(1000,2)
LLN(1000,25)
LLN(1000,100)
LLN(1000,1000)
