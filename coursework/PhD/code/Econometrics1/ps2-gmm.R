#install.packages("gmm")
#install.packages("dplyr")
library(gmm)
library(dplyr)
set.seed(1)

# Load your data
data <- read.csv("./Metrics1-phd-2024/ps2/gmmdata.csv")

# Calculate the average of r
r_bar <- mean(data$r)

# Define the moment conditions
g <- function(theta, data) {
  beta <- theta[1]
  gamma <- theta[2]
  ct <- data$c[-length(data$c)]
  ct_plus_1 <- data$c[-1]
  rt_plus_1 <- data$r[-1]
  
  m1 <- beta * rt_plus_1 * ct_plus_1^(gamma - 1) - ct^(gamma - 1)
  m2 <- ct^(gamma - 1) * (rt_plus_1 - r_bar)
  
  return(cbind(m1, m2))
}

# Define the gradient
grad_g <- function(theta, data) {
  beta <- theta[1]
  gamma <- theta[2]
  ct <- data$c[-length(data$c)]
  ct_plus_1 <- data$c[-1]
  rt_plus_1 <- data$r[-1]
  
  grad_1 <- ct_plus_1^(gamma - 1) * rt_plus_1
  grad_2 <- beta * (log(ct_plus_1)) * ct_plus_1^(gamma - 1) * rt_plus_1 - (log(ct)) * ct^(gamma - 1)
  grad_3 <- 0
  grad_4 <- (rt_plus_1 - r_bar) * (log(ct)) * ct^(gamma - 1)
  
  return(matrix(c(grad_1, grad_2, grad_3, grad_4), ncol = 2))
}

# Initial parameters
init_params <- c(beta = 0.96, gamma = 1.1)

# Perform the GMM estimation
result <- gmm(g, x = data, t0 = init_params, grad = grad_g, type = "iterative")

# Display the results
print(summary(result))


