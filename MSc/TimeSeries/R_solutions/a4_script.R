library(haven)
library(ggplot2)
library(vars)
library(lmtest)
library(xtable)
library(reshape2)
library(aTSA)
library(urca)
library(dplyr)

#% Schwert's rule
schwert <- function (T_) {
  floor(12 * (T_ / 100)^(1/4))
}

#% Sequential t-rule
sequential_t_rule <- function(series) {
  
  exog = na.omit(lag(series,1))
  series = diff(series)
  max_lag = schwert(length(series))
  
  p_opt = 1
  
  for (p in seq(max_lag,1,-1)) {
    
    lags <- seq(1:p)
    xnam <- paste0("I(lag(series, ", lags, "))")
    (fmla <- as.formula(
      paste(paste("series ~ ", paste(xnam, collapse= "+")),"+ exog"))
    )
    model = lm(fmla)
    t_stats = coef(model) / (sqrt(diag(vcov(model))))
    t = t_stats[length(t_stats) - 1]
    pval = 2*pt(-abs(t), df = (p + 2))
    print(sprintf("Order: %i T: %.3f p: %.3f", p, t, pval))
    
    if (pval < 0.05) {
      p_opt = p
      break
    }
  }
  print(sprintf("Sequential t rule picks %i", p_opt))
  return(p_opt)
}

# Set the plotting scheme 
gridded <- theme_linedraw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5),
        axis.line = element_line(colour = "black"),
        legend.position="bottom",
        text=element_text(size=10,  family="sans"))

# Read in the data
df = read_dta("../../a4/computation/Data_A4.dta")
df$year = 1960.0 + .25 * 0:(nrow(df) - 1)

dff = melt(df, id.vars = "year")
ggplot(data = dff, aes(x = year, y = value, group = variable)) +
  geom_line(aes(color = variable, linetype = variable)) +
  gridded
#ggsave("../../a4/computation/cointegrated.png", dpi = 300)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 2a)
p = sequential_t_rule(df$tbill)
p = 7 # From textbook
adf.test(df$tbill,p + 1)$type1

p = sequential_t_rule(df$r10)
p = 5 # From textbook
adf.test(df$r10,p + 1)

p = sequential_t_rule(df$r5)
p = 7 # From textbook
adf.test(df$r5,p + 1)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2b) Engle-Granger test

lmmodel = lm("tbill ~ r5 + r10", data = df)
summary(lmmodel)
e = residuals(lmmodel)
p = sequential_t_rule(e)

exog = na.omit(lag(e,1))
y = diff(e)
m = lm(y ~ lag(y,1) +  lag(y,2) +  lag(y,3) +  lag(y,4)  +  lag(y,5) 
   +  lag(y,6) +  lag(y,7) +  lag(y,8) + exog - 1)
summary(m)
adf.test(e, p + 1)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2c)

model = lm("r10 ~ r5 + tbill", data = df)
summary(model)
e = residuals(model)
p = sequential_t_rule(e)
adf.test(e,p + 1)

exog = na.omit(lag(e,1))
y = diff(e)
m = lm(y ~ lag(y,1) +  lag(y,2) +  lag(y,3) +  lag(y,4)  +  lag(y,5) 
       +  lag(y,6) + exog - 1)
summary(m)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2d) Johnansen procedure

# Trace
jotest=ca.jo(df[,c("tbill","r5","r10")], ecdet = "const", K=8, type = "trace")
summary(jotest)

# Eigen
jotest=ca.jo(df[,c("tbill","r5","r10")], ecdet = "const", K=8, type = "eigen")
summary(jotest)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2e)

# % Tbill - r10 pairing
model = lm("tbill ~ r10", data = df)
e = residuals(model)
p = sequential_t_rule(e)
adf.test(e, p + 1);

model = lm("r10 ~ tbill", data = df)
e = residuals(model)
adf.test(e, p + 1);


# % Tbill - r5 pairing
model = lm("tbill ~ r5", data = df)
e = residuals(model)
adf.test(e, p + 1);

model = lm("r5 ~ tbill", data = df)
e = residuals(model)
p = sequential_t_rule(e)
adf.test(e, p + 1);

# % r10 - r5 pairing
model = lm("r5 ~ r10", data = df)
summary(model)
e = residuals(model)
p = sequential_t_rule(e)
out = adf.test(e, p + 1)$type1[6,2:3]

model = lm("r10 ~ r5", data = df)
e = residuals(model)
p = sequential_t_rule(e)
adf.test(e, p + 1);
