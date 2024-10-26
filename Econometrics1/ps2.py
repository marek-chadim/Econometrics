"""
This script solves the simulation exercises for Problem Set 2.
"""
# Import packages
import numpy as np
import matplotlib.pyplot as plt
import math
from scipy.stats import norm
import matplotlib.pyplot as plt

np.random.seed(1)

# Path 
out_path = "/Users/thomasmikaelsen/Documents/Teaching/Metrics1-phd-2024/ps2"
###############################################################################

### Problem 3(e)
ns = [50, 100, 250, 1000]
xtildes = [0.5, 1, 2, 3]

# List of 16 matrices, one for each combination of n and xtilde
# Each matrix has 4 columns, one for each estimator, and 500
# rows, one for each simulation
estimations = np.empty((16,500,4))

def F_exp(x,l):
    return 1 - math.exp(-l*x)

def F_norm(x, mu, sigma):
    return norm.cdf(x, loc=mu, scale=sigma)

for n in ns:
    for xtilde in xtildes:
        for i in range(500):
            draws = np.random.exponential(1,n) 
            x_bar = np.sum(draws) / n 
            sd = np.std(draws) 
            mu_hat_mle = x_bar 
            sigma_sq_hat_mle = np.sum((draws - mu_hat_mle)**2) / n

            e1 = F_exp(xtilde,1/x_bar)
            e2 = F_exp(xtilde, 1/(2 * x_bar) + 1/(2 * sd))
            e3 = F_norm(xtilde, mu_hat_mle, sigma_sq_hat_mle)
            e4 = np.sum(draws <= xtilde) / n

            # Subtract the true value of F_exp(xtilde,1) from each estimator -- gives us the bias of each estimator for each observation
            estimations[ns.index(n)*4 + xtildes.index(xtilde),i,0] = e1 - F_exp(xtildes.index(xtilde),1)
            estimations[ns.index(n)*4 + xtildes.index(xtilde),i,1] = e2 - F_exp(xtildes.index(xtilde),1)
            estimations[ns.index(n)*4 + xtildes.index(xtilde),i,2] = e3 - F_exp(xtildes.index(xtilde),1)
            estimations[ns.index(n)*4 + xtildes.index(xtilde),i,3] = e4 - F_exp(xtildes.index(xtilde),1)

# Empirical variance of each estimator: 16 by 4 matrix (one row for each (n,xtilde) combination and one column for each estimator)
variances = np.var(estimations, axis=1)

# 16 by 4 matrix (one row for each (n,xtilde) combination and one column for each estimator)
biases = np.mean(estimations, axis=1)

# Mean squared errors = bias^2 + var
mses = biases**2 + variances

# LaTeX formatted names of the four estimators
names = [r"$F_{\exp}(\lambda_{\text{MLE}})$", r"$F_{\exp}(\lambda_{2})$", r"$F_{\text{norm}}$", r"$\text{Share}$"]

######### Plotting #########
### Variance graphs 
# Reshape variances for easier plotting
reshaped_variances = variances.reshape((len(ns), len(xtildes), -1))

# Create a figure with 4 subplots (one for each xtilde)
fig, axs = plt.subplots(2, 2, figsize=(10, 8))
axs = axs.flatten()

# Loop over each xtilde
for xtilde_index, xtilde in enumerate(xtildes):
    ax = axs[xtilde_index]

    # Plot variances for each estimator for the current xtilde
    for estimator_index, name in enumerate(names):
        ax.plot(ns, reshaped_variances[:, xtilde_index, estimator_index], label=name)

    ax.set_xlabel('n')
    ax.set_ylabel('Variance')
    ax.set_title(f'Variance for xtilde = {xtilde}')
    ax.legend()

plt.tight_layout()
plt.savefig(f'/Users/thomasmikaelsen/Documents/Teaching/Metrics1-phd-2024/ps2/variance_graphs_combined.png')
plt.show()



### Bias graphs
# Reshape biases for easier plotting
reshaped_biases = biases.reshape((len(ns), len(xtildes), -1))

# Create a figure with 4 subplots (one for each xtilde)
fig, axs = plt.subplots(2, 2, figsize=(10, 8))
axs = axs.flatten()

# Loop over each xtilde
for xtilde_index, xtilde in enumerate(xtildes):
    ax = axs[xtilde_index]

    # Plot biases for each estimator for the current xtilde
    for estimator_index, name in enumerate(names):
        ax.plot(ns, reshaped_biases[:, xtilde_index, estimator_index], label=name)

    ax.set_xlabel('n')
    ax.set_ylabel('Bias')
    ax.set_title(f'Bias for xtilde = {xtilde}')
    ax.legend()

plt.tight_layout()
plt.savefig('/Users/thomasmikaelsen/Documents/Teaching/Metrics1-phd-2024/ps2/bias_estimators_combined.png')
plt.show()


### MSE graphs
# Reshape mses for easier plotting
reshaped_mses = mses.reshape((len(ns), len(xtildes), -1))

# Create a figure with 4 subplots (one for each xtilde)
fig, axs = plt.subplots(2, 2, figsize=(10, 8))
axs = axs.flatten()

# Loop over each xtilde
for xtilde_index, xtilde in enumerate(xtildes):
    ax = axs[xtilde_index]

    # Plot MSEs for each estimator for the current xtilde
    for estimator_index, name in enumerate(names):
        ax.plot(ns, reshaped_mses[:, xtilde_index, estimator_index], label=name)

    ax.set_xlabel('n')
    ax.set_ylabel('MSE')
    ax.set_title(f'MSE for xtilde = {xtilde}')
    ax.legend()

plt.tight_layout()
plt.savefig('/Users/thomasmikaelsen/Documents/Teaching/Metrics1-phd-2024/ps2/mse_estimators_combined.png')
plt.show()








