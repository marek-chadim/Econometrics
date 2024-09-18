#install.packages("sandwich")
install.packages("lmtest")
library(lmtest)
library(sandwich)
library(dplyr)


# Read in the data
data <- read.csv("mitaData.csv")

# Construct polynomial variables for latitude and longitude
data <- mutate(data, x_2 = x * x, 
               y_2 = y * y,
               xy = x * y,
               x_3 = x * x * x,
               y_3 = y * y * y,
               x2_y = x * x * y,
               x_y2 = x * y * y)

# Construct polynomial variables for distance to Potisi
data <- mutate(data, dpot_2 = dpot * dpot,
               dpot_3 = dpot * dpot * dpot)

# Subset by distance to border
data_100km <- filter(data, d_bnd<=100) 
data_75km <- filter(data, d_bnd<=75)
data_50km <- filter(data, d_bnd<=50)

## Define a function for returning the variance covariance matrix
get_CL_vcov <- function(model, cluster){
  
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  
  uj <- apply(estfun(model),2,function(x) tapply(x,cluster,sum))
  
  vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
  return(vcovCL)
  
}

## Define a function for number of degrees of freedom, with clustering
get_CL_df <- function(model, cluster){
  M <- length(unique(cluster))
  df=M-1
  return(df)
}

# Using latitude and longitude
# Run regressions, 100km
m1_100km <- lm(lhhequiv ~ pothuan_mita + x + y + x_2 + y_2 + xy + x_3 + y_3 + x2_y + x_y2 + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data=data_100km)
m1_100km_vcov=get_CL_vcov(m1_100km,data_100km$district)
m1_100km_df=get_CL_df(m1_100km,data_100km$district)
coeftest(m1_100km, m1_100km_vcov, df=m1_100km_df)

# Run regressions, 75km
m1_75km <- lm(lhhequiv ~ pothuan_mita + x + y + x_2 + y_2 + xy + x_3 + y_3 + x2_y + x_y2 + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data=data_75km)
m1_75km_vcov=get_CL_vcov(m1_75km,data_75km$district)
m1_75km_df=get_CL_df(m1_75km,data_75km$district)
coeftest(m1_75km, m1_75km_vcov, df=m1_75km_df)

# Run regressions, 50km
m1_50km <- lm(lhhequiv ~ pothuan_mita + x + y + x_2 + y_2 + xy + x_3 + y_3 + x2_y + x_y2 + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data=data_50km)
m1_50km_vcov=get_CL_vcov(m1_50km,data_50km$district)
m1_50km_df=get_CL_df(m1_50km,data_50km$district)
coeftest(m1_50km, m1_50km_vcov, df=m1_50km_df)

## Using distance to Potosi
# Run regressions, 100km
m2_100km <- lm(lhhequiv ~ pothuan_mita + dpot + dpot_2 + dpot_3 + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data=data_100km)
m2_100km_vcov=get_CL_vcov(m2_100km,data_100km$district)
m2_100km_df=get_CL_df(m2_100km,data_100km$district)
coeftest(m2_100km, m2_100km_vcov, df=m2_100km_df)

# Run regressions, 75km
m2_75km <- lm(lhhequiv ~ pothuan_mita + dpot + dpot_2 + dpot_3 + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data=data_75km)
m2_75km_vcov=get_CL_vcov(m2_75km,data_75km$district)
m2_75km_df=get_CL_df(m2_75km,data_75km$district)
coeftest(m2_75km, m2_75km_vcov, df=m2_75km_df)

# Run regressions, 50km
m2_50km <- lm(lhhequiv ~ pothuan_mita + dpot + dpot_2 + dpot_3 + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data=data_50km)
m2_50km_vcov=get_CL_vcov(m2_50km,data_50km$district)
m2_50km_df=get_CL_df(m2_50km,data_50km$district)
coeftest(m2_50km, m2_50km_vcov, df=m2_50km_df)