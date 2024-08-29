#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                   FUNCTIONS

# % Partial autocorrelations
durbin_levinson <- function(r) {
  #This function computes the partial autocorrelation estimates using the Durbin-Levinson Algorithm.
  #Inputs: r - vector containing autocorrelation estimates (including autocorrelation)
  
  # Retrieve lag-order
  q = length(r) - 1
  
  #Initialize empty vector to store coefficients
  phi = vector(, length = q)
  # Set first (partial) autocorrelation
  phi = r[2]
  
  #This formulation follows p.65 in Enders (2015)
  theta = c(r[2])
  for (s in 2:q) {
    
    cov_partial = r[s + 1] - t(theta) %*% rev(r[2:s])
    var_partial = r[1] - t(theta) %*% r[2:s]
    theta_s = cov_partial / var_partial
    
    phi[s] = as.numeric(theta_s)
    
    theta = c(theta - c(theta_s) * rev(theta), theta_s)
  }
  
  phi = append(1., phi)
  
  return(phi)
  
}

# % Autocorrelations

acfd <- function(y, q) {
  
  T_ = length(y)
  y = y - mean(y)
  
  gamma = vector(, length = q)
  gamma[1] = (y %*% y) / (T_ - 1)
  for (k in 1:q){
    # Actually I am not sure the denominator is right here, but
    # this delivers the same results as the built-in acf function
    gamma[k + 1] = y[(k + 1):T_] %*% y[1:(T_ - k)] / (T_ - 1)
  }
  rho = gamma / gamma[1]
  
  return(rho)
}


# %%%% Ljung-Box Test Statistic %%%% #

ljungbox <- function(x, q, df) {
  
  r = acfd(x, q)
  T_ = length(x)
  Q = sum(r[2:length(r)]**2 / c(T_ - 1:(length(r) - 1))) * T_ *(T_ + 2)
  
  return(Q)
}


# %%% Chow Test %%% #

chow <- function(model, data, t_break){
  
  #This function implements the Chow-test
  #It takes in as inputs
  # model   - model that you want to run
  # data    - a piece of data (needs to be a time-series object in this case)
  # t_break - a break point, at which the sample is split. Note that the quarter you
  #           specify as t_break will be the last quarter included in the first sample.

  # 1. Part
  # % Without breaks (entire sample)
  
  # Fit the model
  fitted = model(data)
  n = length(fitted$coef)
  residuals = residuals(fitted) 
  residuals = residuals[residuals != 0]
  T_ = length(residuals)
  
  # Obtain the residuals (forecast errors), square them and sum them
  # This gives you the sum of squared forecast errors/residuals
  ssr = sum(residuals ** 2)
  
  
  # 2. 
  # % With structural breaks
  
  # Fit the model to the pre-break period
  data_pre = window(data, start = c(y0,q0), end = t_break)
  fitted = model(data_pre)
  residuals = residuals(fitted) 
  residuals = residuals[residuals != 0]
  ssr_pre = sum(residuals ** 2)
  
  # Fit the model to the post-break period
  data_post = window(data, start = t_break + c(0,1), end = c(y1,q1))
  fitted = model(data_post)
  ssr_post = sum(fitted$residuals ** 2)
  
  # Compute the F-statistic
  ssr_ratio = (ssr - ssr_pre - ssr_post) / (ssr_pre + ssr_post)
  chow_stat = (T_ - 2*n) / n * ssr_ratio
  
  # Print the results to the terminal
  # The sprintf function formats your string / print output.
  # For example, if you tell it sprintf(%.3f,x), the numerical value x
  # will be printed as a floating-point number with 3 decimal places.
  print(sprintf("Chow-Test with %i - 2 * %i = %i",T_,n,(T_ - 2*n)))
  print(sprintf("Pre: %.2f Post: %.2f Total: %.2f", ssr_pre, ssr_post, ssr))
  print(sprintf("F-test: %.3f", chow_stat))
  
  v <- c(T_ - 2*n, ssr_pre, ssr_post, ssr, chow_stat) 
  return(v)
}


# %%% CUSUM Test %%% #


cusum_test <- function(data, model, k) {
  
  T_ = length(data) - 1
  Tn = T_ - k
  
  Y = data[2:length(data)]
  X = cbind(rep(1, T_),data[1:T_])
  
  w = vector(, length = Tn)
  for (r in (k + 1):T_) {
    
    X_r_1 = X[1:(r - 1),]
    X_r_inv = solve(t(X_r_1) %*% X_r_1)
    b_1 = X_r_inv %*% (t(X_r_1) %*% Y[1:(r - 1)])
    
    X_r = X[1:r,]
    y_r = Y[r]
    
    y_hat = X_r[r,] %*% as.vector(b_1)
    
    divisor = sqrt(1 + as.numeric(t(X_r[r, ]) %*% X_r_inv %*% X_r[r, ]))
    
    e_r = (y_r - y_hat) / divisor
    
    w[r - k] = as.numeric(e_r)
  }
  
  ssr = sum(w ** 2)
  sigma_eps = sqrt(ssr / (length(w) - 2))
  cusum = cumsum(w) / sigma_eps
  
  cusumsq = cumsum(w ** 2) / ssr
  
  bw = 0.948 * sqrt(Tn) + 2 * (1:Tn) / sqrt(Tn)
  lb = -1 * bw
  ub = 1 * bw
  
  c_s = .24245
  lb_sq = (1:Tn / Tn) - c_s
  ub_sq = (1:Tn / Tn) + c_s
  
  df = data.frame(obs = (k + 1):T_,
                  cusum = cusum,
                  lb = lb,
                  ub = ub,
                  cusumsq = cusumsq,
                  lb_sq = lb_sq,
                  ub_sq = ub_sq)
  return(df)
}



cusum_test <- function(data, model, n) {
  
  T_ = length(data)
  Tn = T_ - n 
  
  cusum = vector(, length = Tn)
  cusumsq = vector(, length = Tn)
  
  for (i in 0:length(cusum)) {
    fitted = arima(data[1:(n + i)], model)
    cusum[i + 1]  = sum(fitted$residuals)
    cusumsq[i + 1]  = sum(fitted$residuals ** 2)
  }
  
  cusum = cusum / sqrt(var(fitted$residuals))
  cusumsq = cusumsq / cusumsq[Tn]
  
  bw = 0.948 * sqrt(T_ - n) + 2 * (n:T_ - n) / sqrt(T_ - n)
  lb = -1 * bw
  ub = 1 * bw
  
  c_s = .24245
  lb_sq = (0:Tn / Tn) - c_s
  ub_sq = (0:Tn / Tn) + c_s
  
  df = data.frame(obs = n:T_,
                  cusum = cusum,
                  lb = lb,
                  ub = ub,
                  cusumsq = cusumsq,
                  lb_sq = lb_sq,
                  ub_sq = ub_sq)
  return(df)
}

# %%% Printing regression table %%% #

print_table <- function(labels, results) {
  
  sink("./tables/table1.tex", split = TRUE)
  
  cat(sprintf("\\begin{tabular}{c ccccccc}"))
  cat(sprintf("\\hline "))
  cat(sprintf("\\hline "))
  cat(sprintf("\\rule{0pt}{3ex} "))
  cat(sprintf(" & AR(7) & AR(6) & AR(2) & p = 1,2,7 & ARMA(1,1) & ARMA(2,1) & \\specialcell[b]{p = (1,2) \\\\ q = (1,7)}  \\\\"))
  cat(sprintf("\\hline "))
  cat(sprintf("\\rule{0pt}{3ex} "))
  
  for (i in 1:nrow(results)) {
    
    vals = as.character(results[i,])
    vals[is.na(vals)] = " "
    vals = paste(paste("$",vals, sep=""),"$", sep = "")
    
    if ((i %% 2 == 0) & (i <= nrows * 2)){
      vals = paste(paste("(",vals, sep=""),")", sep = "")
    }
    vals[vals == "($ $)"] = ""
    v1 = c(labels[i], paste(" & ",vals, sep="")," \\\\ ")
    out = paste(v1,collapse="")
    cat(sprintf(out))
  }
  cat(sprintf("\\hline "))
  cat(sprintf("\\hline "))
  cat(sprintf("\\end{tabular}"))
  sink()
  closeAllConnections()
  
  
}
  
  
# 
export_dat <- function(v, path) {
jj = 0
for (i in v) {
  # Write the number to the file
  i = sprintf("%.2f",i)
  writeLines(as.character(i), paste(path, jj, ".dat", sep = ""))
  jj = jj + 1
}
}

digit_to_dat <- function(i, path) {
  writeLines(as.character(i), paste(path,".dat", sep = ""))
}
