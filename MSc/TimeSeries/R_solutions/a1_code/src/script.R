#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                       PREAMBLE

setwd("/Users/dominik/Dropbox/teaching/time_series/assignments/a1")

library(reshape2)
library(ggplot2)
library(latex2exp)

#element_rect(colour = "black", fill=NA, linewidth=0.5),
# Set some parameters for plotting
graph_settings <- theme_linedraw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position="bottom",)

#Fix aspect ratios and height
aspect_ratio = 1.618 # Golden Ratio
h = 5

export = TRUE

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a0 = 2
a2 = .5
a22 = .6
y00 = 2
y01 = 1.5
y11 = 2

digit_to_dat(a0,"./numbers/a0")
digit_to_dat(a2,"./numbers/a2")
digit_to_dat(a22,"./numbers/a22")
digit_to_dat(y00,"./numbers/y00")
digit_to_dat(y01,"./numbers/y01")
digit_to_dat(y11,"./numbers/y11")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%                      MAIN

# %%% Part 1 %%% #
# Call the script for simulation of the difference equation

source("./src/functions.R")


# %%% Part 2 %%% #
#% Loading the data and computing first descriptives

data = read.table("./data/US_spread.txt", sep = ",", header = TRUE);
#data <- data[(data$year <= 2008) & (data$quarter <= 1),]
data = data[order(data$year, data$quarter),]
y0 <- data[1,"year"]
q0 <- data[1,"quarter"]
y1 <- data[length(data$spread),"year"]
q1 <- data[length(data$spread),"quarter"]
data = ts(data$spread, start = c(y0, q0), end = c(y1,q1), freq = 4)
nT = length(data)

digit_to_dat(nT, "./numbers/T")

#Taking first-differences
fdiff = diff(data)
#Compute the mean for first-difference: This is indicative of growth
mean(fdiff)

# I will put this into a dataframe as this turns out to be quite convenient for plotting
df = data.frame(spread = as.vector(data), time = time(data))
df$spread_mean = mean(df$spread)
df$spread_diff = append(NA, as.vector(fdiff))

# %%% Autocorrelations

# Set lag order 
q = 12

r = acfd(as.vector(data), q)

# Optional: Compute confidence intervals for autocorrelation
# T_ = length(data)
# ar_r = (1 * 2*cumsum(r**2)) / T_
# std_r = sqrt(var_r)
# ub = r + std_r * 1.96
# lb = r - std_r * 1.96

pacf_yw = durbin_levinson(r)

correlogram = data.frame(lag = 0:q, acf = r, pacf = pacf_yw)
correlogram = melt(correlogram, id = "lag", variable = "type")

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# 2a. Replicating the table

# The way I recreate the table here is maybe not the best,
# as I essentially write the latex code as a set of string outputs
# which are send to a tex-file. Other options like stargazer
# probably do a better job, but I quite like to have full control
# over how the output looks.

# Specify the maximal lags that we will encounter
p = 7
q = 7
mn = 7
nrows = p + q + 1

# Now specify which coefficients will be "active" in each model.
# The matrix R has 15 rows corresponding to (q + p) + 1 for the maximum number of
# coefficients that the model can have. We have mn models to compute.
# Now fill the matrix with ones wherever we want to estimate a coefficient in the model.

R = matrix(, nrow = nrows, ncol = mn)
R[nrows, ] = 1
R[c(1:7),1] = 1
R[c(1:6),2] = 1
R[c(1:2),3] = 1
R[c(1,2,7),4] = 1
R[c(1,8),5] = 1
R[c(1,2,8),6] = 1
R[c(1,2,8,14),7] = 1

l = nrows * 2
results = matrix(, nrow = nrows * 2 + 6, ncol = mn);
for (i in 1:mn) {
  
  # In the loop we initialize the a vector of restrictions (the restriction will be that
  # the coefficient is set to zero)
  # For each element in the i-th row that contains a one, we want to impose no restriction.
  restrictions = rep(0, nrows)
  restrictions[R[,i] == 1] = NA
  
  # Now estimate the ARMA model with these restrictions.
  fitted = arima(data, order = c(p,0,q), fixed = restrictions)
  
  # Take out the intercept and all the coefficients and put them into a 
  # matrix to store the results
  results[1,i] = fitted$coef["intercept"]
  results[1 + 2 * (2:nrows - 1),i] = fitted$coef[1:(nrows - 1)]
  
  # Now generate all your favorite diagnostic tools and store them.
  k = length(fitted$coef)
  T_ = nobs(fitted) - k + 1

  ssq = sum(fitted$residuals ** 2)
  results[l + 1,i] = ssq
  results[l + 2,i] = -2 * fitted$loglik + 2*k
  results[l + 3,i] = -2 * fitted$loglik + k * log(T_)
  results[l + 4,i] = ljungbox(fitted$residuals, 4, 0)
  results[l + 5,i] = ljungbox(fitted$residuals, 8, 0)
  results[l + 6,i] = ljungbox(fitted$residuals, 12, 0)
  
  # Now get the t-statistics
  nz_idx = which(fitted$coef != 0)
  nzcoef = fitted$coef[nz_idx]
  stde = sqrt(diag(fitted$var.coef))
  tstat = nzcoef / stde
  idx = nz_idx[1:(length(nz_idx) - 1)]
  results[4 + 2*(idx - 1),i] = tstat[names(idx)]
  results[2,i] = tstat["intercept"]

}

results[results == 0] = NA
results = round(results, digits = 2)

# Create some labels for the first column
labels = vector(, length = nrows * 2)
for (i in 1:p) {
  labels[3 + 2 * (i - 1)] = paste0("AR(", i, ")")
}
for (i in 1:q) {
  labels[17 + 2 * (i - 1)] = paste0("MA(", i, ")")
}
labels[1] = "Intercept"
labels[seq(2, nrows * 2,2)] = ""
labels = append(labels, c("SSR","AIC","BIC","Q(4)","Q(8)","Q(12)"))

# If you call the following function, it will spit out a regression table as in Table 2.4 of the textbook
print_table(labels, results)

#---------------------------------------------------------------------#
# 2b. Chow-Tests 

model1 = function(x) { return(arima(x, c(7,0,0), method = "CSS")) }


model2 = function(x) { 
  return(arima(x, order = c(2,0,7), fixed = c(NA, NA, NA, 0, 0, 0, 0, 0, NA, NA), method = "CSS")) 
}

t_break = c(1981,4)
ch1 <- chow(model1, data, t_break)
ch2 <- chow(model2, data, t_break)

# Now set the structural break at 1988-Q2
ch3 <- chow(model1, data, c(1988,2))
ch4 <- chow(model2, data, c(1988,2))



# For internal use --- Export these numbers to .dat
export_dat(ch1, "./numbers/chow1_")
export_dat(ch2, "./numbers/chow2_")
export_dat(ch3, "./numbers/chow3_")
export_dat(ch4, "./numbers/chow4_")

#---------------------------------------------------------------------#
# 2c. Sensitivity 

n = 10
coefm = matrix(, nrow = length(data) - n + 1, ncol = 4)

for (i in n:nT) {
  
  fitted = arima(data[1:i], c(1,0,0))
  
  j = i - n + 1
  coefm[j,1] = fitted$coef["intercept"]
  coefm[j,2] = fitted$coef["ar1"]
  
  cov_theta = sqrt(diag(fitted$var.coef)) 
  coefm[j,3] = cov_theta["intercept"]
  coefm[j,4] = cov_theta["ar1"]
  
}

res = data.frame(coefm)
colnames(res) <- c("itc","ar1","itc_std","ar1_std")
res$n = n:nT

c = 1.96
res$itc_lb = res$itc - c * res$itc_std
res$itc_ub = res$itc + c * res$itc_std
res$ar1_lb = res$ar1 - c * res$ar1_std
res$ar1_ub = res$ar1 + c * res$ar1_std

#---------------------------------------------------------------------#
# 2d. Endogenous breaks

model = c(1,0,0)
n = 10

cusum_results = cusum_test(data, model, n);
cusum_results$time = as.vector(time(window(data, start = c(1962,2), end = c(2023,4))))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%                      PLOTS

source("./src/simulation.R")

ggplot(df, aes(x = time, y = spread)) +
  geom_line(stat = "identity") +
  geom_hline(yintercept = 0., color = "red") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0.05, 0.05)) + 
  labs(x = "Year", y = "US Interest Rate Spread (%)") +
  theme(panel.border = element_blank()) + 
  graph_settings

if (export) {
ggsave("./figures/A1_02a_spread.pdf",
       height = h , width = h * aspect_ratio, dpi = 300)
}

ggplot(df, aes(x = time, y = spread_diff)) +
  geom_line(stat = "identity") +
  geom_hline(yintercept = 0., color = "red") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0.05, 0.05)) + 
  labs(x = "Year", y = "US Interest Rate Spread (First Difference, %)") +
  theme(panel.border = element_blank()) + 
  graph_settings 

if (export) {
ggsave("./figures/A1_02b_spread.pdf", 
       height = h, width = h * aspect_ratio, dpi = 300)
}

#---------------------------------------------------------------------#
# Plot Figure 2.6

ggplot(correlogram, aes(fill=type, x=lag, y=value)) + 
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_hline(yintercept=0) +
  graph_settings +
  labs(x = "Lag", y = "") +
  scale_y_continuous(breaks=seq(-.25,1.,.25), expand = c(0.05, 0.05)) +
  scale_x_continuous(breaks=seq(0,12,1), expand = c(0, 0)) +
  scale_fill_manual(values= alpha(c("orange","lightblue"),0.6), name = "", labels=c("ACF","PACF")) + 
  theme(legend.position=c(.8,.85), panel.border = element_blank())

if (export) {
ggsave("./figures/A1_03_correlogram.pdf", 
       height = h, width = h * aspect_ratio,
       dpi = 300)
}

#---------------------------------------------------------------------#

ggplot(res, aes(x = n, y = itc)) +
  geom_line(stat = "identity") +
  geom_ribbon(data = res, aes(n, ymin = itc_lb, ymax = itc_ub), alpha = 0.3) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0.05, 0.05)) + 
  labs(x = TeX(r"(Sample size $n$)"), y = TeX(r"(Estimated intercept $\phi_0$)")) + 
  graph_settings

if (export) {
ggsave("./figures/A1_04a_itc.pdf", height = h, width = h * aspect_ratio, dpi = 300)
}

ggplot(res, aes(x = n, y = ar1)) +
  geom_line(stat = "identity") +
  geom_ribbon(data = res, aes(n, ymin = ar1_lb, ymax = ar1_ub), alpha = 0.3) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0.05, 0.05)) + 
  labs(x = TeX(r"(Sample size $n$)"), y = TeX(r"(Estimated AR(1) $\phi_1$)")) + 
  theme(panel.border = element_blank()) + 
  graph_settings

if  (export) {
ggsave("./figures/A1_04b_ar1.pdf", height = h, width = h * aspect_ratio, dpi = 300)
}

#---------------------------------------------------------------------#

cusum_results$lb <- -10
cusum_results$ub <- 10
ggplot(data = cusum_results, aes(x = time, y = cusum)) +
  geom_line(stat = "identity") +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  geom_point(data = cusum_results, aes(time,cusum), color = "black", shape = 21, fill = "white") +
  labs(x = "Observation", y = "Cumulative sum of errors") +
  geom_ribbon(data = cusum_results, aes(time, ymin=lb, ymax=ub), fill= "#56B4E9", alpha = 0.3) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0.05, 0.05)) + 
  theme(panel.border = element_blank()) + 
  graph_settings

if (export) {
ggsave("./figures/A1_05a_cusum.pdf",  height = h, width = h * aspect_ratio, dpi = 300)
}

cusum_results$H0sq = seq(0,1, length.out = nrow(cusum_results)) 
ggplot(data = cusum_results, aes(x = time, y = cusumsq)) +
  geom_line(stat = "identity") +
  geom_line(data = cusum_results, aes(time, H0sq), color = "red") +
  geom_point(data = cusum_results, aes(time,cusumsq), color = "black", shape = 21, fill = "white") +
  labs(x = "Observation", y = "Cumulative sum of squared errors") +
  geom_ribbon(data = cusum_results, aes(time, ymin=lb_sq, ymax=ub_sq), fill= "#56B4E9", alpha = 0.3) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0.01, 0.01)) + 
  graph_settings

if (export) {
ggsave("./figures/A1_05b_cusumsq.pdf",  height = h, width = h * aspect_ratio, dpi = 300)
}