## First install the packages that we need
# We only do this once
install.packages(c("haven", "stargazer", "sandwich","ggplot2", "tidyverse"))

# Then we load them before each session
library(dplyr)
library(haven) # Allows us to import dta files
library(stargazer) # We use this to make a regression table
library(sandwich) # Helps us to compute robust standard errors
library(ggplot2)  # We use this to generate graphs

# Set working directory 
setwd("/Users/jakobbeuschlein/Dropbox/EconometricsI/Datasets")

# Load the data
df <- read_dta("cps09mar.dta")
View(df)

# Next step: regress log earnings on log hours by gender
df_male <- df %>% mutate(log_earnings = log(earnings),
                         log_hours = log(hours)) %>%
  filter(female == 0)

df_female <- df %>% mutate(log_earnings = log(earnings),
                         log_hours = log(hours)) %>%
  filter(female == 1)

# R doesn't like missings, so we want to drop them before
df_male <- df_male %>% tidyr::drop_na() # We can also use packagename:: to call a function from a package
df_female <- df_female %>% tidyr::drop_na()

ols_male <- lm(log_earnings ~ log_hours, data=df_male)
ols_female <- lm(log_earnings ~ log_hours, data=df_female)

table <- stargazer(ols_male, ols_female, type = "text",
                   se = list(vcovHC(ols_male, type = "HC1"), # Choose robust standard errors
                             vcovHC(ols_female, type = "HC1")),
                   title = "OLS Estimates with Robust Standard Errors",
                   align = TRUE)

# Now we run this regression by hand (for women only)
N <- nrow(df_female)
y <- c(df_female$log_earnings)
X <- cbind(rep(1,N), df_female$log_hours)

beta_hat <- solve( t(X)%*%X ) %*% t(X) %*% y
beta_hat

# What is the average earnings by marital status?
table(df$marital)

df_marit <- df %>% group_by(marital) %>%
  summarise(avg_earnings=mean(earnings, na.rm=TRUE))
View(df_marit)

# Plot these averages
plot <- ggplot(df_marit, aes(x = marital, y = avg_earnings)) + # Define variables you want to use
  geom_bar(stat = "identity") + # Define type of graph
  labs(title = "Average Earnings by Marital Status",  # Add further details
       x = "Marital Status",
       y = "Average Earnings") +
  theme_minimal()

plot


