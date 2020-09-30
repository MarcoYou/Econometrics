#========================================================================
#
# Author: AS
# Contributors: 
# Creation:  2020-08-21
# Title: Homework I - On the LLN and on the CLT
#
# Description:
#             
# Note:        
#             
#========================================================================

## 0. load libraries
library(extraDistr)
library(tidyverse)
library(ggplot2)

## 1. DEFINE THE SET-UP [parameters that do not change]
### we need to simulate a rv X mimicking the "roll of a dice", hence we set xmin and xmax
xmin <- 1
xmax <- 6
### correspondingly we compute the true mean and variance

##mean <- (xmin+xmax)/2   [for the discrete uniform, not requested]   
##variance <- (xmax-xmin+1)^2/12 [for the discrete uniform, not requested]   
mean <- -10
variance <- 1

### define the number of samples you want [how many times you run the experiment]
n_samples <- 1000

## 2. DEFINE THE SAMPLE SIZE
n <- 5 #[how many coins you toss at each round]

### 3. DEFINE OBJECTS TO STORE RESULTS 
x <- matrix(nrow=n_samples,ncol=n) #[realizations of your sample]
x_bar <- matrix(nrow=n_samples,ncol=1) #[realizations of sample average]

### 4. RUN THE EXPERIMENT [loop for]
set.seed(123456789)
for (i in 1:n_samples) {
  # generate random draws
  #xi <- rdunif(n,mean,variance) # [discrete uniform distribution ranging between 1 and 6]
  xi <- rnorm(n,mean,variance) # [normal distribution with mean -10 and variance 1]
  #print(xi)
  x[i,]=xi
  xi_bar=mean(xi)
  #print(xi_bar)
  x_bar[i]=xi_bar
}

### 5. IDENTIFY UNIQUE VALUES OF the SAMPLE AVERAGE
x_bar_unique_values  <- unique(x_bar)

### 6. PLOT UNIQUE VALUES of SAMPLE AVERAGE 
### [changing n from 2 to 1000 shows the LLN in action]
plot(x_bar_unique_values,rep(1,length(x_bar_unique_values)),
     axes=FALSE,xlim=c(1,6),
     xlab='values of X_bar', ylab='')+axis(1,seq(1,6,0.5))

### 7. PLOT the DISTRIBUTION of x_bar
### [changing n from 2 to 1000 shows the CLT in action]
data <- data.frame(x_bar_zscore=(x_bar-mean)/sqrt(variance/n))
ggplot(data, aes(x = x_bar_zscore)) + 
  geom_histogram(aes(y=..density..),bins=24) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1))


