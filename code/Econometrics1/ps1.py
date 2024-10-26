"""
This script solves the simulation exercises for Problem Set 1.
"""
# Import packages
import numpy as np
import matplotlib.pyplot as plt
import math

np.random.seed(1)
###############################################################################
### Problem 4.2(a)
beta = 1/3 
k = 40
sds = []

for N in [50,100,250,500,1000]:
    beta_hats = []
    for i in range(0,100):
        # Draw number of boys in each class
        x = np.random.binomial(k,1/2,N)
        # Fraction of each class that are boys
        m = x / k
        # Errors
        eps = np.random.normal(0,1,N)
        # Teacher effort 
        y = beta * m + eps
        # beta_hat
        beta_hats.append(np.dot(m,y) / np.dot(m,m))
        
    sds.append(np.std(beta_hats))

# Plot sds as function of N
plt.plot([50,100,250,500,1000],sds)
plt.show()


### Problem 4.2(b)
sds = []

for k in [10, 20, 40, 60]:
    for N in [50,100,250,500,1000]:
        beta_hats = []
        for i in range(0,100):
            # Draw number of boys in each class
            x = np.random.binomial(k,1/2,N)
            # Fraction of each class that are boys
            m = x / k
            # Errors
            eps = np.random.normal(0,1,N)
            # Teacher effort 
            y = beta * m + eps
            # beta_hat
            beta_hats.append(np.dot(m,y) / np.dot(m,m))
            
        sds.append(np.std(beta_hats))

# Turn sds into 4x5 matrix
sds = np.reshape(sds,(4,5))

# Plotting
k_values = [10, 20, 40, 60]
N_values = [50,100,250,500,1000]
plt.figure(figsize=(10, 6))
for i, k in enumerate(k_values):
    plt.plot(N_values, sds[i, :], label=f'k = {k}')

plt.xlabel('N')
plt.ylabel('Standard Deviation')
plt.title('Standard Deviations for Different k Values as a Function of N')
plt.legend()
plt.grid(True)
plt.show()

### Problem 4.3(ii)
# Bionmial mass function 
def binom_pmf(k,n,p):
    return math.factorial(n) / (math.factorial(k) * math.factorial(n-k)) * p**k * (1-p)**(n-k)

result = sum(binom_pmf(k,10,0.5) for k in [0,1,2,8,9,10])

### Problem 4.3(iii)
result = sum(binom_pmf(k,20,0.5) for k in [0,1,2,3,4,16,17,18,19,20])

### Problem 4.3(iii)
# Floor of 60/4 
lb = (np.floor(60/4)-1).astype(int)
ub = (np.ceil((3*60)/4)+1).astype(int)
k_values = list(range(0,lb+1)) + list(range(ub,61))

result = sum(binom_pmf(k,60,0.5) for k in k_values)