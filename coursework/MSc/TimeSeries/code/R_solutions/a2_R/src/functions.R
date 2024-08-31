# ==================================================================
# Subroutines for Assignment 2: Applied TSA
# Dominik Wehr
# Jan 27th 2024
# ==================================================================


# ------------------------------------------------------------------
# %%% Schwert's Rule
# Input: T_ - Length of time series
schwert <- function (T_) {
  floor(12 * (T_ / 100)^(1/4))
}

# ------------------------------------------------------------------
# %%% Utility: Create additional lags
# This function simply adds a lot of lags to the data-set
DFLags <- function(df, p_max) {
  
  vars <- sapply(seq(1:p_max), function(i){ paste0("dy_l",i) })
  
  for (i in 1:p_max) {
    coln = vars[i]
    df = df %>% mutate(!!coln := lag(dy, n = i, order_by = time))
  }
  
  return(df)
}

# ------------------------------------------------------------------
# %%% Utility: Lag Table for model selection
# This function is used to compute AICs and BICs
LagTable <- function(df, p_max) {
  
  IC = matrix(, nrow = p_max, ncol = 4)
  
  
  for (p in 1:p_max) {
    
    print(p)
    k =  p + 2
    model = lm(paste("dy ~ 0 + y_l1 + ",lag_poly("dy",p)), data = df)
    
    IC[p,1] = AIC(model)
    IC[p,2] = BIC(model)
    IC[p,3] = Box.test(model$residuals, type = "Ljung-Box", 4)$statistic
    IC[p,4] = Box.test(model$residuals, type = "Ljung-Box", 8)$statistic
    
  }
  
  return(IC)
}

# ------------------------------------------------------------------
# %%% General to specific approach
# This function implements the general-to-specific approach
GTS <- function(df, p_max) {
  
  for (p in p_max:1) {
    
    print(sprintf("Lag order %i:", p))
    model = lm(paste("dy ~ 0 + y_l1 + ",lag_poly("dy",p)), data = df)
    se = sqrt(diag(vcov(model)))
    
    k = length(model$coef)
    t = model$coef[k] / se[k]
    
    pval = 2*pt(-abs(t), df = model$df.residual)
    
    print(sprintf("T-statistic: %.2f", t))
    print(sprintf("P-value: %.4f", pval))
    if (pval < 0.1) {
      break 
    }
    
  }
  
}

# ------------------------------------------------------------------
# %%% Utility: Lag Polynomial Formula
# Just a utility for writing out the formula for lag polynomial
lag_poly <- function(x,order) {
  
  s = paste(x,"_l1", sep = "")
  
  if (order >= 2) {
    for (i in 2:order) {
      s = paste(s," + ",x,"_l",i, sep = "")
    }
  }
  return(s)
  
}
