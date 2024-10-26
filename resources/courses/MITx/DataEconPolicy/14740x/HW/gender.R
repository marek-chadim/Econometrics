#install.packages("lfe")
library(lfe)
library(lattice)
library(dplyr)

# Set workspace
setwd("data")

# Load the data
teaData <- read.csv(file="tea_data.csv")
colnames(teaData)

# Create dummies and interaction term for tea sown and post period
teaData <- mutate(teaData, teaDum = as.numeric(teaData$teasown>0))
teaData <- mutate(teaData, post = as.numeric(teaData$biryr>=1979))
mean(teaData$teaDum)
mean(teaData$post)
teaData$teaDumPost <- teaData$teaDum*teaData$post

# Run regression to estimate share of males born
reg1 <- lm(sex ~ teaDumPost + teaDum + post, teaData)
summary(reg1)

# Create orchard and cashcrop dummies and interaction terms
teaData <- mutate(teaData, orcDum = as.numeric(teaData$orch>0))
teaData <- mutate(teaData, cashDum = as.numeric(teaData$cashcrop>0))
teaData$orcDumPost <- teaData$orcDum*teaData$post
teaData$cashDumPost <- teaData$cashDum*teaData$post

# Run regression to estimate share of males born
reg2 <- lm(sex ~ teaDumPost + orcDumPost + cashDumPost + teaDum + orcDum + cashDum + post, teaData)
summary(reg2)

# Run regression with fixed effects 
reg3 <- felm(sex ~ teaDumPost + orcDumPost + cashDumPost | biryr + admin, teaData)
summary(reg3)
