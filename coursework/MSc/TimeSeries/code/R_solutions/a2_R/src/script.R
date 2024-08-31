# ==================================================================
# Subroutines for Assignment 2: Applied TSA
# Dominik Wehr
# Jan 27th 2024
# ==================================================================


# ==================================================================
#                         PACKAGES
# ==================================================================

library(ggplot2)
library(dplyr)
library(aTSA)
library(urca)
library(reshape2)
library(latex2exp)
library(xtable)

source("./functions.R")

# ==================================================================
#                         GRAPHICAL SETTINGS
# ==================================================================

# Set some parameters for plotting
graph_settings <- theme_linedraw() + 
  theme(panel.border = element_rect(colour = "white", fill=NA, linewidth=0.05),
        panel.grid.major = element_line(color = alpha("gray", 0.4)),
        panel.grid.minor = element_line(color = alpha("gray", 0.4)), 
        axis.line = element_line(colour = "black", linewidth = 0.5),
        legend.position="bottom",
        text=element_text(size=10,  family="sans"))

#These are the graphic settings for ggplot for a gridded plot
gridded <- theme_linedraw() + 
  theme(panel.border = element_rect(colour = "white", fill=NA, linewidth=0.05),
        panel.grid.major = element_line(color = alpha("gray", 0.4), linewidth = 0.1),
        panel.grid.minor = element_line(color = alpha("gray", 0.4), linewidth = 0.1), 
        axis.line = element_line(colour = "black", linewidth = 0.5),
        legend.position="bottom",
        text=element_text(size=10,  family="sans"))

#Fix aspect ratios and height
aspect_ratio = 14/9
h = 5

export = TRUE

# ==================================================================
#                   EXERCISE 1
# ==================================================================

# %%% 1. Exercise
# %%% Swedish exchange rates

# Read in the data
data = read.table("../data/A2_SEK_US_ExchangeRates.txt");

# For plotting it can be useful to transform it into a time series object
# You set start and end dates and specify the frequency
# In our case it is annual frequency so I set freq = 1
data = ts(data, start = c(1973,1), end = c(2018,1), freq = 1)

# We start with a visual inspection of the data
# You could simply call plot(data)
# However, I generally prefer to use ggplot2, which requires me
# to put the data into a data.frame object

# I retrieve the length of the time series
T_ = length(data)

# Now I put everything into a dataframe
# Note that time(time-series-object) gives me the years as a vector
df = data.frame(xrate = as.vector(data), 
                time = time(data))

# This would be one way to obtain the lag
# You need to add a missing value (NA) in the beginning
df$xrate_l1 = c(NA, data[1:(length(data) - 1)])


# 1a: Plot the series
# Note: I cannot go into the details here on how to use ggplot2.
# There are many good guides out there.

ggplot(data = df, aes(x = time, y = xrate)) + 
  geom_line(stat = "identity") + 
  geom_point(stat = "identity") +
  labs(x = "Year", y = "Exchange rate (SEK/US)") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0.01, 0.01)) + 
  gridded

if (export) {
ggsave("../figures/A2_01a_xrate.pdf", height = h, width = h * aspect_ratio, dpi = 300)
}


ggplot(data = df, aes(x = xrate_l1, y =xrate)) + 
  geom_point(stat = "identity") +
  geom_smooth(method='lm', formula= y~x, color = "red", fill = alpha("red", 0.2)) +
  scale_x_continuous(expand = c(0, 0.1)) + scale_y_continuous(expand = c(0.01, 0.01)) + 
  labs(x = TeX("Exchange rate in $t - 1$"), y = TeX("Exchange rate in $t$")) +
  gridded

if (export) {
  ggsave("../figures/A2_01b_xrate_scatter.pdf", height = h, width = h * aspect_ratio, dpi = 300)
}

# ==================================================================
# 2. %%%%%
# ==================================================================

# 2.a Settling on a maximal lag

# Get max lag from Schwert's rule
p_max = schwert(T_)

# Now put the data into a dataframe
df = data.frame(y = as.vector(data), time = time(data))

# In order to add one lag to the dataframe, use 
# the function mutate from dplyr
# order_by = time ensures that your data is sorted according to time (ascending)
df = mutate(df, y_l1 = lag(y, order_by = time))

# Compute a first difference using this newly obtained lag
df$dy = df$y - df$y_l1

# This function simply adds a lot of first differences to the dataset
df = DFLags(df, p_max)

# Call the "lag-table"
table1 = LagTable(df, p_max)
round(table1, digits = 2)

GTS(df, p_max)

# Based on the results I pick lag order p = 1.
# I just run the model and compute the Ljung-Box tests again.
# Now I can take a look at the p-values of this test from the output in console.
# I also obtain a full summary of the regression results

k = 1
model = lm(paste("y ~ 0 + y_l1 + ",lag_poly("dy",k)), data = df)
Box.test(model$residuals, lag = 4, fitdf = k + 1)
Box.test(model$residuals, lag = 8, fitdf = k + 1)
summary(model)


k = 5
model = lm(paste("y ~ time + y_l1 + ",lag_poly("dy",k)), data = df)
Box.test(model$residuals, lag = 4)
Box.test(model$residuals, lag = 8)
summary(model)

# This computes the adf_rho statistic
adf_rho = 42 * (model$coef["y_l1"] - 1) / (1 - model$coef["dy_l1"])
print(adf_rho)

covm = sqrt(diag(vcov(model)))
ADF_t = (model$coef["y_l1"] - 1) / covm["y_l1"]
print(ADF_t)

# Elliott, Rothenberg & Stock Unit Root Test
# I will now implement the ERS Test as described in my solutions.
# Alternatively skip ahead and use the utility from the urca package.

# First, this requires you to specify a near unit
# Here the near unit root value is around ~ 0.7
alph = 1 - (13.5 / T_)

# Set up a design matrix for detrending
z = cbind(rep(1,T_),seq(1:T_))

# Now detrend this variable. The first value is set to z_1.
Z = rbind(z[1,], z[2:T_,] - alph * z[1:(T_-1),])

# De-trend the original series
Y = df$y - alph * lag(df$y,1)
Y[1] = df$y[1]

# Now perform a linear projection (OLS regression)
beta_gls = as.vector(solve(t(Z) %*% Z) %*% (t(Z) %*% Y))

# Detrend the original variable by computing the residuals
df$y_tilde = df$y - z %*% beta_gls

# Now compute lags and first differences
df$y_tilde_l1 = lag(df$y_tilde,1)
df$dy_tilde = df$y_tilde - df$y_tilde_l1
df$dy_tilde_l1 = lag(df$dy_tilde)
df$dy_tilde_l2 = lag(df$dy_tilde,2)
df$dy_tilde_l3 = lag(df$dy_tilde,3)
df$dy_tilde_l4 = lag(df$dy_tilde,4)
df$dy_tilde_l5 = lag(df$dy_tilde,5)

# Run the following regression without intercept (!)
model = lm("y_tilde ~  y_tilde_l1 + dy_tilde_l1  + dy_tilde_l2 + dy_tilde_l3 + dy_tilde_l4 + dy_tilde_l5 -1", data = df)
summary(model)

# The ERS test statistic is given by
ers_stat = (model$coef[1] - 1)/ sqrt(diag(vcov(model)))[1]
print(ers_stat)

# This is my visualization of what happens when you detrend the variables in this way.
# Skip ahead
gls = data.frame(time = df$time, 
                 dy_t = df$dy_tilde,
                 dy = df$dy)

gls = gls[2:nrow(gls),]
gls = melt(gls, id = "time", variable = "series")
ggplot(data = gls, aes(x = time, y = value, color = series)) +
  geom_line(aes(color = series)) +  geom_point(aes(color = series)) +
  geom_point(fill = "white", size = 2., shape = 21) +
  labs(x = "Time", y = "First difference") +
  scale_linetype_manual(values=c("solid", "longdash")) +
  scale_color_manual(values=c("#CC6677","#6699CC"), name = "", labels=c(TeX("$dy_t$"),TeX("$dy$"))) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0.01, 0.01)) + 
  gridded + theme(legend.position=c(.1,.15), panel.border = element_blank())

if (export) {
ggsave("../figures/A2_02a_gls.pdf", height = h, width = h * aspect_ratio, dpi = 300)
}


# ==================================================================
#             3. Industrial Production
# ==================================================================

# Load the two datasets
uk_data = read.table("../data/A2_IP_US.txt");
uk_data = ts(uk_data, start = c(1960,1), end = c(2015,4), freq = 4)

us_data = read.table("../data/A2_IP_UK.txt");
us_data = ts(us_data, start = c(1960,1), end = c(2015,4), freq = 4)

# For visualization it will be useful to put these two in one data.frame
df = data.frame(y0 = as.vector(uk_data),
                y1 = as.vector(us_data),
                time = as.vector(time(us_data)))
df = melt(df, id = "time", variable = "Country")
levels(df$Country) = c("UK","US")

# Plot the two series
ggplot(data = df, aes(x = time, y = value, group = Country)) +
  geom_line(linewidth = 0.8, aes(linetype = Country, color = Country)) +
  labs(x = "Year", y = "Industrial production") +
  scale_linetype_manual(values=c("solid", "longdash")) +
  scale_color_manual(values=c("#CC6677","#6699CC")) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0.01, 0.01)) + 
  annotate("text", x=1990, y=60, label= "UK", colour = "#CC6677") + 
  annotate("text", x=1980, y=97.5, label= "US", colour = "#6699CC") + 
  gridded + 
  theme(legend.position="none")

if (export) {
ggsave("../figures/A2_03a_series.pdf", height = h, width = h * aspect_ratio, dpi = 300)
}
# Optional: Compute confidence intervals for autocorrelation
acfb = function (data, ml) {
  T_ = length(data)
  res = acf(data, lag = ml)
  r = res$acf
  var_r = (1 * 2*cumsum(r**2)) / T_
  std_r = sqrt(var_r)
  ub = r + std_r * 1.96
  lb = r - std_r * 1.96
  return(cbind(seq(1:(ml + 1)), r,ub,lb))
}

# Set a high lag to get a good overview of the autocorrelations
p_max = 40
coef = rbind(acfb(uk_data, p_max), acfb(us_data, p_max))
df = data.frame(coef)
colnames(df)[1] = "Lag"
df$Country = c(rep("UK",p_max + 1), rep("US", p_max + 1))

ggplot(data = df, aes(x = Lag, y = r, color = Country)) +
  geom_line(aes(color = Country)) +
  geom_point(aes(color = Country)) +
  geom_point(fill = "white", size = 2., shape = 21) +
  labs(x = "Lag", y = "Autocorrelation") +
  scale_linetype_manual(values=c("solid", "longdash")) +
  scale_color_manual(values=c("#CC6677","#6699CC")) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0.01, 0.01)) + 
  annotate("text", x=20, y=0.825, label= "UK", colour = "#CC6677") + 
  annotate("text", x=30, y=0.475, label= "US", colour = "#6699CC") + 
  gridded + 
  theme(legend.position="none")

if (export) {
ggsave("../figures/A2_03b_auto.pdf", height = h, width = h * aspect_ratio, dpi = 300)
}

# Get maximum lag
# The length of both time series is the same
length(uk_data) == length(us_data)
# Therefore we can compute p_max once.
p_max = schwert(length(uk_data))

# Essentially this just prepares the data.frame we need to run our models
# Put everything into data.frame and add lags / differences
df_uk = data.frame(y = as.vector(uk_data), time = time(uk_data))
df_uk = mutate(df_uk, y_l1 = lag(y, order_by = time))
df_uk$dy = df_uk$y - df_uk$y_l1
df_uk = DFLags(df_uk, p_max)

df_us = data.frame(y = as.vector(us_data), time = time(us_data))
df_us = mutate(df_us, y_l1 = lag(y, order_by = time))
df_us$dy = df_us$y - df_us$y_l1
df_us = DFLags(df_us, p_max)


# ---------------------------------------------------------------------
# %%%%% 3.1 UK

# Run the models for the UK
table2 = LagTable(df_uk, p_max)
xtable(round(table2, digits = 2))

GTS(df_uk, p_max)

# We pick p = 2

k = 2
model = lm(paste("y ~ time + y_l1 + ",lag_poly("dy",k)), data = df_uk)
Box.test(model$residuals, lag = 4, fitdf = k, type = "Ljung-Box")
Box.test(model$residuals, lag = 8, fitdf = k, type = "Ljung-Box")
summary(model)

#I will manually compute the ADF test statistics
T_ = nrow(df_uk) - (k + 1)
ADF_rho = T_ * (model$coef["y_l1"] - 1) / (1 - sum(model$coef[4:(3 + k)]))
ADF_t = (model$coef["y_l1"] - 1) / 0.009008

# ERS test
# Here implemented with URCA
uers = ur.ers(df_uk$y, type = "DF-GLS", 
              model = "trend",
              lag.max = 2)
summary(uers)

# Here written out as described in the slides
# I do this so you can see what the detrending does

T_ = length(uk_data)
alph = 1 - (13.5 / T_)
z = cbind(rep(1,T_),seq(1:T_))
Z = rbind(z[1,], z[2:T_,] - alph * z[1:(T_-1),])
Y = df_uk$y - alph * lag(df_uk$y,1)
Y[1] = df_uk$y[1]

beta_gls = as.vector(solve(t(Z) %*% Z) %*% (t(Z) %*% Y))
df_uk$y_tilde = df_uk$y - z %*% beta_gls
df_uk$y_tilde_l1 = lag( df_uk$y_tilde,1)
df_uk$dy_tilde =  df_uk$y_tilde -  df_uk$y_tilde_l1
df_uk$dy_tilde_l1 = lag( df_uk$dy_tilde)
df_uk$dy_tilde_l2 = lag( df_uk$dy_tilde, 2)
mean(df_uk$dy / df_uk$y_l1, na.rm = TRUE)
100 * mean(df_uk$dy / df_uk$y_l1, na.rm = TRUE)
100 * mean(df_uk$dy_tilde / df_uk$y_tilde_l1, na.rm = TRUE)

model = lm("dy_tilde ~  y_tilde_l1 + dy_tilde_l1  + dy_tilde_l2 -1", data = df_uk)
summary(model)

# Plot the first differences from detrending against the original
gls = data.frame(time = df_uk$time, 
                 dy_t = df_uk$dy_tilde,
                 dy = df_uk$dy)

gls = gls[2:nrow(gls),]
gls = melt(gls, id = "time", variable = "series")
ggplot(data = gls, aes(x = time, y = value, color = series)) +
  geom_line(aes(color = series)) +  geom_point(aes(color = series)) +
  geom_point(fill = "white", size = 0.1) +
  labs(x = "Time", y = "First difference") +
  scale_linetype_manual(values=c("solid", "longdash")) +
  scale_color_manual(values=c("#CC6677","#6699CC"), name = "", labels=c(TeX("$dy_t$"),TeX("$dy$"))) +
  scale_x_continuous(expand = c(0.01, 0.01)) + scale_y_continuous(expand = c(0.05, 0.05)) + 
  gridded + theme(legend.position=c(.1,.15))

if (export) {
ggsave("../figures/A2_03c_gls.pdf", height = h, width = h * aspect_ratio, dpi = 300)
}

# ---------------------------------------------------------------------
# %%%%% 3.2 US

# Now run the entire spiel for the US
# Everything is pretty much the same as before, so I will not really comment on this

table3 = LagTable(df_us, p_max)
xtable(round(table3, digits = 2))

model = lm(paste("y ~ time + y_l1 + ",lag_poly("dy",4)), data = df_us)
Box.test(model$residuals, lag = 4, type = "Ljung-Box")
Box.test(model$residuals, lag = 8, type = "Ljung-Box")

ADF_rho = T_ * (model$coef["y_l1"] - 1) / (1 - sum(model$coef[4:(3 + k)]))
covm = sqrt(diag(vcov(model)))
ADF_t = (model$coef["y_l1"] - 1) / covm["y_l1"]

summary(model)

adf.test(df_us$y,5)
uers = ur.ers(df_us$y, type = "DF-GLS", 
              model = "trend",
              lag.max = 5)
summary(uers)


# ---------------------------------------------------------------------
# %%%%% 3.3 Perron-Tests

break_date = 1980.25
df_uk$I_p = as.numeric(df_uk$time == break_date)
df_uk$I_l = as.numeric(df_uk$time >= break_date)
df_uk$I_tau = as.numeric(df_uk$time >= break_date) * df_uk$time

k = 2
model = lm("y ~ time + I_p + I_l", data = df_uk)
summary(model)
Box.test(model$residuals, lag = 4, type = "Ljung-Box")
Box.test(model$residuals, lag = 8, type = "Ljung-Box")
df_uk$fit1 = model$fitted.values

model = lm(paste("y ~ time  + I_p + I_l + y_l1 + ",lag_poly("dy",2)), data = df_uk)
summary(model)
Box.test(model$residuals, lag = 4, type = "Ljung-Box")
Box.test(model$residuals, lag = 8, type = "Ljung-Box")
df_uk$fit2 = c(rep(NA,k + 1),model$fitted.values)
covm = sqrt(diag(vcov(model)))
ADF_t = (model$coef["y_l1"] - 1) / covm["y_l1"]
print(ADF_t)

1 - sum(df_uk$I_l) / nrow(df_uk)


model = lm(paste("y ~ time  + I_l + I_tau + y_l1 + ",lag_poly("dy",2)), data = df_uk)
summary(model)
Box.test(model$residuals, lag = 4, type = "Ljung-Box")
Box.test(model$residuals, lag = 8, type = "Ljung-Box")
df_uk$fit2 = c(rep(NA,k + 1),model$fitted.values)
covm = sqrt(diag(vcov(model)))
ADF_t = (model$coef["y_l1"] - 1) / covm["y_l1"]
print(ADF_t)

model = lm(paste("y ~ time  + I_p + I_l + I_tau + y_l1 + ",lag_poly("dy",2)), data = df_uk)
summary(model)
Box.test(model$residuals, lag = 4, type = "Ljung-Box")
Box.test(model$residuals, lag = 8, type = "Ljung-Box")
df_uk$fit3 = c(rep(NA,k + 1),model$fitted.values)
covm = sqrt(diag(vcov(model)))
ADF_t = (model$coef["y_l1"] - 1) / covm["y_l1"]
print(ADF_t)