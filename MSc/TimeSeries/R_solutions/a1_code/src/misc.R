
# Least-squares estimator for AR(p) process

ols <- function(Y,p) {
  #This function estimates the co AR(p)
  
  idx = 1 + p
  
  T_ = nrow(Y)
  
  X = Y[,idx]
  X = cbind(rep(1,T_), X)
  y = Y[,1]
  coef = solve(t(X) %*% X) %*% (t(X) %*% y)
  
  fitted = X %*% coef
  ssr = sum((y - fitted) ** 2)
  
  aic = T_ * log(ssr) + 2 * (length(p) + 1)
  print(ssr)
  print(aic)
  
  return(coef)
}


pacf_ols <- function(Y, q) {
  
  coef = vector(, (q + 1))
  coef[1] = 1.0
  for (k in 1:q) {
    coef[k + 1] = ols(Y,c(1:k))[k + 1]
  }
  
  return(coef)
}






dmatrices <- function(data, q) {
  
  T = length(data)
  
  Y = matrix(, nrow = T - q, ncol = q + 1);
  for (i in 0:q) {
    Y[,i+ 1] = data[(q + 1 - i):(T - i)];
  }
  
  return(Y)
  
}


yule_walker_toeplitz <- function(r){
  
  q = length(r) - 1
  
  R = matrix(, nrow = q, ncol = q)
  
  R[1, ] = head(r,q)
  for (i in 2:(q - 1)) {
    print(i)
    r0 = rev(head(r,i))
    r1 =  head(r, q + 1 - i )
    R[i, ] = c(r0, r1[2:length(r1)])
  }
  R[q, ] = rev(head(r,q))
  
  return(R)
}


pacf_yw <- function(r,R) {
  # Using repeated linear projections 
  q = length(r) - 1
  phi = vector(, length = q)
  phi[1] = r[2]
  for (i in 2:q) {
    coef = solve(R[1:i,1:i]) %*% r[2:(i + 1)]
    phi[i] = coef[i]
  }
  
  return(phi)
}