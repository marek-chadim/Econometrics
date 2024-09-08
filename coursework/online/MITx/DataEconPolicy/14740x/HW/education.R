#install.packages("AER")
library(AER)

# Set workspace
setwd("data")

# Read in the data
data <- read.csv("inpres_data.csv")

# OLS regression
summary(lm(log_wage~education, data=data))

# DD Tables: Education
data$exposed <- data$birth_year>67
mean(data[data$high_intensity==0&data$exposed==FALSE,"education"])
mean(data[data$high_intensity==0&data$exposed==TRUE,"education"])
mean(data[data$high_intensity==1&data$exposed==FALSE,"education"])
mean(data[data$high_intensity==1&data$exposed==TRUE,"education"])

# DD Tables: Wages
mean(data[data$high_intensity==0&data$exposed==FALSE,"log_wage"])
mean(data[data$high_intensity==0&data$exposed==TRUE,"log_wage"])
mean(data[data$high_intensity==1&data$exposed==FALSE,"log_wage"])
mean(data[data$high_intensity==1&data$exposed==TRUE,"log_wage"])

# DD by regression
data$exp_int <- data$exposed*data$high_intensity
summary(lm(education~exp_int+exposed+high_intensity, data=data))
summary(lm(log_wage~exp_int+exposed+high_intensity, data=data))

# IV version
summary(ivreg(log_wage~education+exposed+high_intensity|exp_int+exposed+high_intensity, data=data))

