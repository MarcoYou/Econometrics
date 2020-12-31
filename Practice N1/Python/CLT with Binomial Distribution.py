%matplotlib inline
import numpy as np
import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
import scipy.special as sp
from scipy.stats import norm
from random import *
plt.style.use('seaborn-whitegrid')

def CLT(size, reps):
    np.random.seed(10)
    mean_x = np.array
    x = np.random.binomial(n = 1, p = 0.5, size = (reps,size))
    dfx = pd.DataFrame(x)
    return(dfx.mean(axis = 1)) # returns a Series of means

dfx_mean = pd.DataFrame(CLT(100,100), columns = ['mean(x)']) # transformation to single column matrix
sample_mean_x = np.mean(CLT(100,100))
sample_std_x = np.std(CLT(100,100))

fig, ax = plt.subplots(nrows = 2, ncols = 2, figsize=(10, 5)) # be careful with the dimension of variable ax

## Matplotlib
# number of counts in each bin, left edge of each bin, each bin(rectangle)
n, bins, patches = ax[0,0].hist(CLT(100,100), 10) 
# y_value = ((1 / (np.sqrt(2 * np.pi) * sigma)) * np.exp(-0.5 * (1 / sigma * (bins - mu))**2))
y_value = ((1 / (np.sqrt(2 * np.pi) * sample_std_x)) * np.exp(-0.5 * (1 / sample_std_x * (bins - sample_mean_x))**2))
ax[0,0].axvline(x = sample_mean_x, color = 'red')
ax[0,0].axis(xmin = sample_mean_x - .2, xmax = sample_mean_x + .2)
ax[0,0].plot(bins, y_value) # should draw a fitted distribution curve :(
ax[0,0].set(xlabel='X ~ Binomial', ylabel='counts')

## Seaborn
sns.distplot(CLT(100,100), bins = 10, ax = ax[0,1], norm_hist = True,  # sample, rep size = 100
             fit = norm, kde = True, rug = True) # norm function needs to be imported
ax[0,1].axvline(x = sample_mean_x, color = 'red')
ax[0,1].axis(xmin = sample_mean_x - .2, xmax = sample_mean_x + .2)
ax[0,1].set(xlabel='X ~ Binomial', ylabel='counts')

sns.distplot(CLT(100,1000), bins = 10, ax = ax[1,0], norm_hist = True, # rep size increased
             fit = norm, kde = True, rug = True) # norm function needs to be imported
ax[1,0].axvline(x = sample_mean_x, color = 'red')
ax[1,0].axis(xmin = sample_mean_x - .2, xmax = sample_mean_x + .2)
ax[1,0].set(xlabel='X ~ Binomial', ylabel='counts')

sns.distplot(CLT(1000,100), bins = 10, ax = ax[1,1], norm_hist = True, # sample size increased
             fit = norm, kde = True, rug = True) # norm function needs to be imported
ax[1,1].axvline(x = sample_mean_x, color = 'red')
ax[1,1].axis(xmin = sample_mean_x - .2, xmax = sample_mean_x + .2)
ax[1,1].set(xlabel='X ~ Binomial', ylabel='counts')

fig.tight_layout()