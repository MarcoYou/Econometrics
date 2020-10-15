%matplotlib inline
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
plt.style.use('seaborn-whitegrid')
from random import *

def LLN(iteration, sample_size):
    
    pop_mean = (1+2+3+4+5+6)/6
    mean_dice = []
    y_value = np.full(100, 1/6)
    
    for i in range(iteration):
        dice_array = np.random.randint(1, 7, (1, sample_size))
        mean_dice.append(np.mean(dice_array))
        
    mean_array = np.array(mean_dice)
    sample_mean = np.mean(mean_array)

    plt.tight_layout()
    
    plt.plot(mean_array, y_value, 'o')
    plt.axvline(x = pop_mean, color = 'red')
    plt.axvline(x = sample_mean, color = 'blue')
    plt.xlim(1, 6)
    plt.ylim(0, 1)
    plt.show()

LLN(100, 2)
LLN(100, 25)
LLN(100, 100)
LLN(100, 1000)
