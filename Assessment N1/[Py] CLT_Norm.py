%matplotlib inline
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
plt.style.use('seaborn-whitegrid')
from random import *

# population mean and standard deviation
mu, sigma = -10, 1
# Normal sample
s = np.random.normal(mu, sigma, 1000)
z = (s - np.mean(s))/np.sqrt(sigma**2/1000)

count, bins, ignored = plt.hist(s, 30, density = True)
plt.plot(bins,
         1/(sigma * np.sqrt(2 * np.pi)) * np.exp( - (bins - mu)**2 / (2 * sigma**2) ),
         linewidth = 2,
         color = 'r')
plt.xlim(-15, -5)
plt.show()
