library(stargazer)
# 1.  Generate artificial data
## set seed
set.seed(123)
## normal distribution parameters
mu1   <- 50; mu2   <- 100
sd1   <- sqrt(10); sd2   <- sqrt(10)
cov12 <- 5
cor12 <- (cov12/(sd1*sd2))
## bivariate normal distribution parameters
mu    <- c(50,100)
sigma <- matrix(c(sd1^2, sd1*sd2*cor12, sd2*sd1*cor12, sd2^2),2)
## random sample generation
N  <- 1000
XY <- mvrnorm(N,mu,sigma)
X  <- XY[, 1]
Y  <- XY[, 2]
## measurement error
w  <- rnorm(N, mean =  0, sd = 10)
Xt <- X + w
# regression with correct sample
regress1 <- lm(formula = Y ~ X)
stargazer(regress1, type="latex") # to generate the summary table in LaTeX format
beta.01 <- coef(regress1)         # intercept and slope of regress1 stored in beta.01
beta.0 <- as.numeric(beta.01["(Intercept)"])
beta.1 <- as.numeric(beta.01["X"])
# plot regress1
par(mfrow=c(2,2))
plot(X, Y, col = "red", xlab = "X", main = "True Observation", cex=.5)
abline(beta.01, lwd = 2)
# regression with errored sample
regress2 <- lm(formula = Y ~ Xt)
stargazer(regress2, type="latex")
beta.hat.01 <- coef(regress2)
beta.hat.0 <- as.numeric(beta.hat.01["(Intercept)"])
beta.hat.1 <- as.numeric(beta.hat.01["Xt"])
# plot regress2
plot(Xt, Y, col = "red", xlab = "X tilda", main = "Measurement Error", cex=.5)
abline(beta.hat.01, lwd = 2)
# ANOVA test
anova(regress1)
anova(regress2)
# beta.hat probability convergence check
check1 <- ((var(X) + var(w))/var(X)) * beta.hat.1 # sample variances
beta.1 - check1
# beta corrected
beta.co.1 <- ((var(X) + var(w))/var(X)) * beta.hat.1
# if var(w) -> 0 then beta.1 = beta.hat.1
# if var(w) -> oo then (var(X)/(var(X)+var(w))) -> 0 hence beta.hat.1 -> 0

## Monte Carlo simulation
# bias
set.seed(NULL)
fixed.samp <- 100
iter <- c(25,100,500,1000)
B.bias <- rep(0,length(iter))
for(i in 1:length(iter)){
  Bb <- rep(0,iter[i])
  for(j in 1:iter[i]){
    xy <- mvrnorm(fixed.samp,mu,sigma)
    x  <- xy[, 1]
    y  <- xy[, 2]
    wt <- rnorm(fixed.samp, mean =  0, sd = 10)
    xt <- x + wt
    
    reg.monte <- lm(formula = y ~ xt)
    beta.01.monte <- coef(reg.monte)
    beta.1.monte <- as.numeric(beta.01.monte["xt"])
    
    Bb[j]   <- beta.1.monte
  }
  mean.Bb <- mean(Bb)
  B.bias[i] <- mean.Bb
}
# accuracy of mean does not improve

# consistency
fixed.iter <- 100
samp <- c(25,100,500,1000)
B.cons <- rep(0,length(samp))
for(i in 1:length(samp)){
  Bc <- rep(0,fixed.iter)
  for(j in 1:fixed.iter){
    xy <- mvrnorm(samp[i],mu,sigma)
    x  <- xy[, 1]
    y  <- xy[, 2]
    wt <- rnorm(samp[i], mean =  0, sd = 10)
    xt <- x + wt
    
    reg.monte <- lm(formula = y ~ xt)
    beta.01.monte <- coef(reg.monte)
    beta.1.monte <- as.numeric(beta.01.monte["xt"])
    
    Bc[j]   <- beta.1.monte
  }
  sd.Bc <- sd(Bc)
  B.cons[i] <- sd.Bc
}
# variance decreases