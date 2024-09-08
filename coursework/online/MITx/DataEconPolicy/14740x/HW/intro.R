# Set workspace
setwd("data")
# Read in the data
data <- read.csv("wb_dev_ind.csv")
# Create illiteracy rates
data$illiteracy_female = 100 - data$literacy_female
data$illiteracy_male   = 100 - data$literacy_male
data$illiteracy_all    = 100 - data$literacy_all
# Summarize the data
names <- data.frame(data$countrycode,data$countryname)
data$countrycode <- NULL
data$countryname <- NULL
summary(data)
# Get standard deviation, ignoring missing values
sapply(data, sd, na.rm=TRUE)
# Take the portion where gdp_per_capita isn't missing
data2 <- subset(data, !is.na(data$gdp_per_capita))
# 50 richest countries
data2 <- data2[with(data2,order(-gdp_per_capita)),]
summary(head(data2,n=50))
sapply(head(data2, n=50), sd, na.rm=TRUE)
# 50 poorest countries
summary(tail(data2,n=50))
sapply(tail(data2, n=50), sd, na.rm=TRUE)
# Median GDP per capita
summary(data)
# Run Regressions
summary(lm(infant_mortality ~ gdp_per_capita, data=data))
summary(lm(illiteracy_all ~ gdp_per_capita, data=data))
# Regress and make scatter plot
mort_lit_line <- lm(infant_mortality ~ illiteracy_all, data=data)
plot(data$illiteracy_all, data$infant_mortality)
abline(mort_lit_line)
summary(mort_lit_line)

