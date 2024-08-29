# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(haven)
library(ggplot2)
library(vars)
library(lmtest)
library(xtable)
library(dplyr)
library(reshape2)

# Set the plotting scheme 
gridded <- theme_linedraw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position="bottom",
        text=element_text(size=10,  family="sans"))

# %%% Utilities

# =======================================================================
#                 I. THEORETICAL PART
# =======================================================================

# %%%%%% I. a) Subroutines %%%%%%%

# This is the code for generating the impulse response function
# You take the reduced-form matrix A, the inverse of the triangular matrix B,
# A vector of shocks and the number of periods used for the impulse response (n)
impulse <- function(A,B_inv,eps,n) {
  
  eps_norm = B_inv %*% eps
  path = matrix(, nrow = 2, ncol = n)
  path[,1] = eps_norm
  for (i in 2:n) {
    path[,i] = A %*% path[,i - 1]
  }
  return(path)
  
}


# This function provides an alternative way of deriving the impulse responses
# It uses the eigendecomposition as explained in my notes
# Both functions deliver the same impulse response

eig_impulse <- function(A,B_inv,eps,n) {
  
  eig = eigen(A)
  Lambda = diag(eig$values)
  V = eig$vectors
  V_inv = solve(V)


  eps_norm = B_inv %*% eps
  path = matrix(, nrow = 2, ncol = 5)
  path[,1] = eps_norm
  for (i in 2:5) {
    path[,i] = A %*% path[,i - 1]
  }
  
  return(path)
}


# %%%%%% I. b) Solution %%%%%%%

# Initialize matrices
A = matrix(c(.5, .4, .2, .3), nrow = 2, ncol = 2)
B = matrix(c(1., 0., -.25, 1.), nrow = 2, ncol = 2)
B_inv = solve(B)

# 1.a First impulse response
A = matrix(c(.5, .4, .2, .3), nrow = 2, ncol = 2)
impulse(A, B_inv, c(1,0), 5)

# 1.b Verify eigenvector approach
eig_impulse(A,B_inv,c(1,0),5)

# 2. Different shock structure
impulse(A, B_inv, c(0,1), 5)

# 3.a Unit Root I
A = matrix(c(1,0,0,1), nrow = 2, ncol = 2)
impulse(A, B_inv, c(1,0), 5)

# 3.b Unit Root II
A = matrix(c(0,1,1,0), nrow = 2, ncol = 2)
impulse(A, B_inv, c(1,0), 5)



# =======================================================================
#                 II. EMPIRICAL
# =======================================================================

# Read in the data
df = read_dta("Data_A3.dta")

# Compute the spread
df$s = df$r10 - df$tbill
# Compute log differences in industrial production
df$lip = c(NA,diff(log(df$indpro)))
# Compute difference in unemployment rates
df$ur = c(NA,diff(df$urate))

# Set the ordering
# Rate spread comes first, then unemployment, then industrial production
ordering = c("s","ur","lip")

# Generate the appropriate matrix
X = df[2:nrow(df),c("s","ur","lip")]

# Now run the VAR with this matrix and set maximum lag to three
var <- VAR(X, p = 3)

# Retrieve the residuals of the VAR
e = residuals(var)

# Now plot the correlation of the residuals
cor(e)

# Perform Granger test for s Granger causing lip
grangertest(lip ~ s, order = 3, data = X)

# Granger test for s causing u
grangertest(ur ~ s, order = 3, data = X)

# This computes the variance decomposition
fevdtable = fevd(var)

# The following is merely for plotting / LaTeX Table purposes
pt = c(1,5,9)
xtable(round(100 * fevdtable$lip[pt,], digits = 2))
xtable(round(100 * fevdtable$ur[pt,], digits = 2))
xtable(round(100 * fevdtable$s[pt,], digits = 2))


# Now switch up the ordering
ordering = c("s","urate","indpro")
X2 = df[1:nrow(df), ordering]

var(X2)
#X2[,2:3] = X2[,2:3] / diag(var(X2))[2:3]
X2$indpro = X2$indpro / 100
var2 <- VAR(X2, p = 5)

# Retrieve the residuals and run a multivariate Ljung-Box test
resids = residuals(var2)
serial.test(var2)

# You can also compute the variance decomposition if you like
fevdtable2 = fevd(var2)

xtable(round(100 * fevdtable2$s[pt,], digits = 2))
xtable(round(100 * fevdtable2$urate[pt,], digits = 2))
xtable(round(100 * fevdtable2$indpro[pt,], digits = 2))


summary(var)
# Std 0.01236
var(X)

summary(var2)
# Std 0.7545
var(X2)

# Impulse response functions
# No onion-cutting here, just take the irf function from the var package
irf1 = irf(var, n.ahead = 20)

# Reverse the ordering using rev()
X = df[2:nrow(df), rev(c("s","ur","lip"))]
var3 <- VAR(X, p = 3)
irf3 = irf(var3, n.ahead = 20)

dframe = df[c("s","lip","urate")]
dframe$lip = dframe$lip * 100
dframe$date = 1960 + (214 / 4) * (seq(1:214) - 1) / 214
dframe = melt(dframe, id = "date")

ggplot(data = dframe, aes(x = date, y = value, group = variable)) + 
  geom_line(aes(color = variable, linetype = variable)) +
  labs(x = "Year", y = "Value") + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0.05, 0.05)) + 
  gridded
ggsave("./Plots.png", dpi = 300)

dff = data.frame(lag = rep(seq(0:20) - 1,2), 
                  coef = c(irf1$irf$lip[,"ur"],irf3$irf$lip[,"ur"]),
                  coef_lb = c(irf1$Lower$lip[,"ur"],irf3$Lower$lip[,"ur"]),
                  coef_ub = c(irf1$Upper$lip[,"ur"],irf3$Upper$lip[,"ur"]),
                  model = c(rep("Model 1",21),rep("Model 2",21))
                  )

ggplot(data = dff, aes(x = lag, y = coef, group = model)) + 
  geom_line(aes(color = model), linewidth = 1.25) +
  geom_ribbon(aes(ymin = coef_lb, ymax = coef_ub, fill = model, alpha = 0.3)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0.05, 0.05)) + 
  labs(x = "Periods after initial shock", y = "Change in unemployment") + 
  gridded
ggsave("./IRF.png", dpi = 300)