
set.seed(1995)

##====================Common Settings=====================
# Sample size & Degrees of freedom
N <- 100
dof <- N - 1
# Sample drawing
x <- rt(N, dof)
u <- rt(N, dof)
# Model Generation
true.b0 <- sqrt(2)
true.b1 <- sqrt(3)
y <- true.b0 + true.b1*x + u
# Graphical environment construction
par(mfrow = c(3,2))
##========================================================

##============Regression with R built-in Command==============
Computational_Regression <- function(size, rep, intercept, slope){
  ## Regress y as response, x as predictor using lm R built-in function
  regress <- lm(formula = y ~ x)
  summary(regress)
  ## Receive OLS-estimated coefficients of the model
  b.hat.lm <- coef(regress)
  b0.hat.lm <<- as.numeric(b.hat.lm["(Intercept)"]) # make it global
  b1.hat.lm <<- as.numeric(b.hat.lm["x"])           # make it global
  ## Print regression results
  cat("==Computational Regression==",
      paste("estimated coefs :", b0.hat.lm, b1.hat.lm),
      paste("expected y :", mean(y)),
      paste("expected model :", b0.hat.lm + b1.hat.lm * mean(x)), sep = "\n")
  ## Scatter plot and fitted line with computed coefficients
  plot(x, y, col = "red", xlab = "X ~ T(99) ; U ~ T(99)", main = "R command based regression")
  abline(a = b1.hat.lm, b = b0.hat.lm, col = "blue", lwd = 2)
  ## Monte Carlo Simulation
  ## Set the number of replications and Initialize vectors
  R <- rep
  b0.hat <- rep(0,R)
  b1.hat <- rep(0,R)
  set.seed(1) # for replicability
  ## Draw a new sample each time in the loop and regress each time OLS estimators
  for (i in 1:R){
    X <- rt(size, (size - 1))
    U <- rt(size, (size - 1))
    Y <- true.b0 + true.b1*X + U
    b.hat <- coef(lm(Y ~ X))
    ## Store estimated intercept and slope each time in initialized vectors
    b0.hat[i] <- b.hat["(Intercept)"]
    b1.hat[i] <- b.hat["X"]
  }
  ## Print the mean of vector of estimated slopes (mean of elements of b1.hat vector)
  cat(paste("expected value of b1.hat :", mean(b1.hat)),
      paste("standard deviation of b1.hat :", sd(b1.hat)), sep = "\n")
  ## Plot histogram of all estimated slopes and indicate the position of their mean
  hist(b1.hat, freq=FALSE, xlim=c(1.2,2.2),
       main=(paste("mean(b1.hat) =",round(mean(b1.hat), digits = 3),
                   " sd(b1.hat) =", round(sd(b1.hat), digits = 3))),
       xlab=expression(hat(beta)[1]))
  abline(v = mean(b1.hat), col = "blue", lwd = 2)
  axis(1, at = mean(b1.hat), lab = expression(paste("E[", hat(beta)[1], "]")))
  ## Plot PDF of normal distribution with the mean and the standard deviation of estimated slopes
  xfit<-seq(1.2, 2.2, 0.01)
  lines(xfit,dnorm(xfit,mean(b1.hat),sd(b1.hat)),lwd=2,col="red")
}
##============================================================

##============Manual Regression in Scalar Form================
Scalar_Regression <- function(size, rep, intercept, slope){
  ## Compute OLS estimators manually in scalar form
  b0.hat.scalar <<- mean(y) - (sum(x * y)/sum((x - mean(x))^2)) * mean(x)
  b1.hat.scalar <<- sum((x - mean(x))*(y - mean(y)))/sum((x - mean(x))^2) # COV(X,Y)/V(X)
  ## Print scalar form computation results
  cat("==Scalar Form Regression==",
      paste("estimated coefs :", b0.hat.scalar, b1.hat.scalar),
      paste("expected y :", mean(y)),
      paste("expected model :", b0.hat.scalar + b1.hat.scalar * mean(x)), sep = "\n")
  ## Scatter plot and fitted line with computed coefficients
  plot(x, y, col = "red", xlab = "x ~ T(99) ; u ~ T(99)", main = "Scalar form regression")
  abline(a = b1.hat.scalar, b = b0.hat.scalar, col = "blue", lwd = 2)
  ## Monte Carlo Simulation
  ## Set the number of replications and Initialize vectors
  R <- rep
  b0.hat <- rep(0,R)
  b1.hat <- rep(0,R)
  set.seed(2) # for replicability
  ## Draw a new sample each time in the loop and compute each time OLS estimators in scalar form
  for (i in 1:R){
    X <- rt(size, (size - 1))
    U <- rt(size, (size - 1))
    Y <- true.b0 + true.b1*X + U
    ## Store computed OLS estimators in initialized vectors
    b0.hat[i] <- mean(Y) - (sum(X * Y)/sum((X - mean(X))^2)) * mean(X)
    b1.hat[i] <- sum((X - mean(X))*(Y - mean(Y)))/sum((X - mean(X))^2)
  }
  ## Print the mean of vector of estimated slopes (mean of elements of b1.hat vector)
  cat(paste("expected value of b1.hat :", mean(b1.hat)),
      paste("standard deviation of b1.hat :", sd(b1.hat)), sep = "\n")
  ## Plot histogram of all estimated slopes and indicate the position of their mean
  hist(b1.hat, freq=FALSE, xlim=c(1.2,2.2),
       main=(paste("mean(b1.hat) =",round(mean(b1.hat), digits = 3),
                   " sd(b1.hat) =", round(sd(b1.hat), digits = 3))),
       xlab=expression(hat(beta)[1]))
  abline(v = mean(b1.hat), col = "blue", lwd = 2)
  axis(1, at = mean(b1.hat), lab = expression(paste("E[", hat(beta)[1], "]")))
  ## Plot PDF of normal distribution with the mean and the standard deviation of estimated slopes
  xfit <- seq(1.2,2.2,0.01)
  lines(xfit,dnorm(xfit,mean(b1.hat),sd(b1.hat)),lwd=2,col="red")
}
##============================================================

##============Manual Regression in Matrix Form================
Matrix_Regression <- function(size, rep, intercept, slope){
  ## Transform x, u, and y into matrix form
  X <- as.matrix(cbind(1, x)) # X(100,2)
  U <- as.matrix(u) # U(100,1)
  Y <- as.matrix(y) # Y(100,1)
  ## Compute OLS estimators manually in matrix form
  b.hat.matrix  <- solve((t(X) %*% X)) %*% t(X) %*% Y # beta_matrix = (X'X)^{-1}X'Y
  ## Separate computed Beta matrix of OLS estimators
  b0.hat.matrix <<- b.hat.matrix[1,1]
  b1.hat.matrix <<- b.hat.matrix[2,1]
  ## Print scalar form computation results
  cat("==Matrix Form Regression==",
      paste("estimated coefs :", b0.hat.matrix, b1.hat.matrix),
      paste("expected y :", mean(y)),
      paste("expected model :", b0.hat.matrix + b1.hat.matrix * mean(x)), sep = "\n")
  ## Scatter plot and fitted line with computed coefficients
  plot(x, y, col = "red", xlab = "x ~ T(99) ; u ~ T(99)", main = "Matrix form regression")
  abline(a = b1.hat.matrix, b = b0.hat.matrix, col = "blue", lwd = 2)
  ## Monte Carlo Simulation
  ## Set the number of replications and Initialize vectors
  R <- rep
  B <- as.matrix(c(true.b0, true.b1)) # transform true Beta vector into a matrix
  B0.hat <- rep(0,R)
  B1.hat <- rep(0,R)
  set.seed(3) # for replicability
  ## Draw a new sample each time in the loop and compute each time OLS estimators in matrix form
  for (i in 1:R){
    x.mat <- rt(size, (size - 1))
    X.mat <- as.matrix(cbind(1, x.mat))
    u.mat <- rt(size, (size - 1))
    U.mat <- as.matrix(u.mat)
    Y.mat <- X.mat%*%B + U.mat
    ## Store computed OLS estimators in initialized vectors
    B.hat  <- solve((t(X.mat) %*% X.mat)) %*% t(X.mat) %*% Y.mat
    B0.hat[i] <- B.hat[1,1] # 1st row x 1st column element of B.hat matrix stored in B0.hat vector
    B1.hat[i] <- B.hat[2,1] # 2nd row x 1st column element of B.hat matrix stored in B1.hat vector
  }
  ## Print the mean of vector of estimated slopes (mean of elements of B1.hat vector)
  cat(paste("expected value of b1.hat :", mean(B1.hat)),
      paste("standard deviation of b1.hat :", sd(B1.hat)), sep = "\n")
  ## Plot histogram of all estimated slopes and indicate the position of their mean
  hist(B1.hat, freq=FALSE, xlim=c(1.2,2.2),
       main=(paste("mean(b1.hat) =",round(mean(B1.hat), digits = 3),
                   " sd(b1.hat) =", round(sd(B1.hat), digits = 3))),
       xlab=expression(hat(beta)[1]))
  abline(v = mean(B1.hat), col = "blue", lwd = 2)
  axis(1, at = mean(B1.hat), lab = expression(paste("E[", hat(beta)[1], "]")))
  ## Plot PDF of normal distribution with the mean and the standard deviation of estimated slopes
  xfit<-seq(1.2,2.2,0.01)
  lines(xfit,dnorm(xfit,mean(B1.hat),sd(B1.hat)),lwd=2,col="red")
}
##============================================================

##=================Numerical Optimization=====================
## Minimization function of Residual Sum of Squares
min.RSS <- function(par){
  b0 <- par[1]
  b1 <- par[2]
  RSS <- sum((y - b0 - b1*x)^2)
  return(RSS)
}
## Numerical Optimization function
Numerical_Optimization <- function(intercept, slope){
  ## optim() function by default execute minimization of given function with Nelder-Mead method
  # The algorithm stops if it's unable to reduce the value by a factor of reltol*(abs(val)+reltol) at a step
  result1 <- optim(par = c(1,1), fn = min.RSS, control = list(reltol = 1e-06)) # strict tolerance
  result2 <- optim(par = c(1,1), fn = min.RSS, control = list(reltol = 1)) # loose tolerance
  ## Get optim function results
  b0.hat.opt1 <<- result1$par[1]
  b1.hat.opt1 <<- result1$par[2]
  b0.hat.opt2 <<- result2$par[1]
  b1.hat.opt2 <<- result2$par[2]
  ## Print optimization results
  cat("==Numerical Optimization==",
      paste("estimated coefs with reltol = 1e-06 :", b0.hat.opt1, b1.hat.opt1),
      paste("number of iterations with reltol = 1e-06 :", result1$counts[1]),
      paste("estimated coefs with reltol = 1 :", b0.hat.opt2, b1.hat.opt2),
      paste("number of iterations with reltol = 1 :", result2$counts[1]),
      paste("expected y :", mean(y)),
      paste("epxected model with tolerance 1e-06 :", b0.hat.opt1 + b1.hat.opt1 * mean(x)),
      paste("expected model with tolerance 1 :", b0.hat.opt2 + b1.hat.opt2 * mean(x)), sep = "\n")
  ## Scatter plot and fitted line with computed coefficients of two different tolerance levels
  plot(x, y, col = "red", xlab = "x ~ T(99) ; u ~ T(99)", main = "Numerical Optimization")
  abline(a = b0.hat.opt1, b = b1.hat.opt1, col = "black", lwd = 2)
  abline(a = b0.hat.opt2, b = b1.hat.opt2, col = "grey", lwd = 2)
}
##===========================================================
Computational_Regression(N, 10, true.b0, true.b1)
Computational_Regression(N, 100, true.b0, true.b1)
Computational_Regression(N, 1000, true.b0, true.b1)

Computational_Regression(10, 100, true.b0, true.b1)
Computational_Regression(100, 100, true.b0, true.b1)
Computational_Regression(1000, 100, true.b0, true.b1)

Scalar_Regression(N, 10, true.b0, true.b1)
Scalar_Regression(N, 100, true.b0, true.b1)
Scalar_Regression(N, 1000, true.b0, true.b1)

Scalar_Regression(10, 100, true.b0, true.b1)
Scalar_Regression(100, 100, true.b0, true.b1)
Scalar_Regression(1000, 100, true.b0, true.b1)

Matrix_Regression(N, 10, true.b0, true.b1)
Matrix_Regression(N, 100, true.b0, true.b1)
Matrix_Regression(N, 1000, true.b0, true.b1)

Matrix_Regression(10, 100, true.b0, true.b1)
Matrix_Regression(100, 100, true.b0, true.b1)
Matrix_Regression(1000, 100, true.b0, true.b1)



Numerical_Optimization(true.b0, true.b1)

##===================Verification Step=======================

df <- data.frame(x, y)

Model <- function(x, a, b, u){
  return(b + a*x + u)
}

Squared_Error <- function(data, a, b, u){
  predictions <- with(data, Model(x, a, b, u))
  errors <- with(data, y - predictions)
  return(sum(errors^2))
}

Squared_Error(df, b1.hat.lm, b0.hat.lm, u)
Squared_Error(df, b1.hat.scalar, b0.hat.scalar, u)
Squared_Error(df, b1.hat.matrix, b0.hat.matrix, u)
Squared_Error(df, b1.hat.opt1, b0.hat.opt1, u)
Squared_Error(df, b1.hat.opt2, b0.hat.opt2, u)

a.error <- function(a){
  return(Squared_Error(df, a, 0, u))
}
b.error <- function(b){
  return(Squared_Error(df, 0, b, u))
}
a.curve <- curve(sapply(x, function(a) {a.error(a)}), from = true.b1 - 1, to = true.b1 + 1)
abline(v = true.b1) ; text(10, 500000, expression(beta[1]))
b.curve <- curve(sapply(x, function(b) {b.error(b)}), from = true.b0 - 1, to = true.b0 + 1)
abline(v = true.b0) ; text(10, 400000, expression(beta[0]))
##===========================================================

# High N -> Less Variance Better Mean
# High R -> Only Better Mean




