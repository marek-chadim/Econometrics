############################################################
#          This R Sample Code accompanies the file         #
#               A Very Short Tutorial on R                 #
#    MGTECON 634: Machine Learning and Causal Inference    #
#         Authors: Thai T. Pham & Zanele Munyikwa          #
#                       April, 2016     
############################################################

# In this sample code, we illustrate the use of some
# classification/regression methods with/without using
# regularization

# NOTE: You need to change the path directory below to your own directory for this to run

# Begin by deleting any previously defined variables
rm(list = ls())

# Let's install packages a number of useful packages.
# To make things easy, the following snippet of code will download
# and install everything you'll need.
# But for future reference, remember that to install a package
# you only have to type
# > install.packages("<packagename>")
# And then you can load it with
# > library(lib)

packages <- c("devtools"
  ,"randomForest" 
  ,"rpart" # decision tree
  ,"rpart.plot" # enhanced tree plots
  ,"ROCR"
  ,"Hmisc"
  ,"corrplot"
  ,"texreg"
  ,"glmnet"
  ,"reshape2"
  ,"knitr"
  ,"xtable"
  ,"lars"
  ,"ggplot2"
  ,"matrixStats"
  ,"plyr"
  ,"stargazer")


not_installed <- !packages %in% installed.packages()
if (any(not_installed)) install.packages(packages[not_installed])
lapply(packages,require,character.only=TRUE)
# Now all packages should be installed and loaded!

#library(domc)
#registerDoMC(cores=4) # for a simple parallel computation

#***************************************************************************
# set your working directory
# write the path where the folder containing the files is located

setwd("C:/Users/scaec/Google Drive/MLClass/2018/AEAs/Tutorials")

# Loading data
# We use data from a Social Voting  (paper is attached) experiment
# The data comes in a csv format
filename <- 'socialneighbor.csv'
social <- read.csv(filename)

# some simple print statements 
print(paste("Loaded csv:", filename, " ..."))
colnames(social)

# We generate noise covariates and add them in the data
set.seed(123)
noise.covars <- matrix(data = runif(nrow(social) * 13), 
                       nrow = nrow(social), ncol = 13)
noise.covars <- data.frame(noise.covars)
names(noise.covars) <- c("noise1", "noise2", "noise3", "noise4", "noise5", "noise6",
                         "noise7", "noise8", "noise9", "noise10", "noise11", "noise12","noise13")

# Add these noise covariates to the social data
working <- cbind(social, noise.covars)

# We want to run on a subsample of the data only
# This is the main dataset used in this tutorial
set.seed(333)
working <- working[sample(nrow(social), 20000), ]

# Pick a selection of covariates
# If we have a lot of data and computation power, it is suggested that
# we include all covariates and use regularization. This suggestion is
# based on the observation that it's much easier to fix the overfitting 
# problem than to fix the underfitting problem.
covariate.names <- c("yob", "hh_size", "sex", "city", "g2000","g2002", "p2000", "p2002", "p2004"
                     ,"totalpopulation_estimate","percent_male","median_age", "percent_62yearsandover"
                     ,"percent_white", "percent_black", "median_income",
                     "employ_20to64", "highschool", "bach_orhigher","percent_hispanicorlatino",
                     "noise1", "noise2", "noise3", "noise4", "noise5", "noise6",
                     "noise7", "noise8", "noise9", "noise10", "noise11", "noise12","noise13")

# The dependent (outcome) variable is whether the person voted, 
# so let's rename "outcome_voted" to Y
names(working)[names(working)=="outcome_voted"] <- "Y"

# Extract the dependent variable
Y <- working[["Y"]]

# The treatment is whether they received the "your neighbors are voting" letter
names(working)[names(working)=="treat_neighbors"] <- "W"

# Extract treatment variable & covariates
W <- working[["W"]]
covariates <- working[covariate.names]

# some algorithms require our covariates be scaled
# scale, with default settings, will calculate the mean and standard deviation of the entire vector, 
# then "scale" each element by those values by subtracting the mean and dividing by the sd
covariates.scaled <- scale(covariates)
processed.unscaled <- data.frame(Y, W, covariates)
processed.scaled <- data.frame(Y, W, covariates.scaled)


# some of the models in the tutorial will require training, validation, and test sets.
# set seed so your results are replicable 
# divide up your dataset into a training and test set. 
# Here we have a 90-10 split, but you can change this by changing the the fraction 
# in the sample command
set.seed(44)
smplmain <- sample(nrow(processed.scaled), round(9*nrow(processed.scaled)/10), replace=FALSE)

processed.scaled.train <- processed.scaled[smplmain,]
processed.scaled.test <- processed.scaled[-smplmain,]

y.train <- as.matrix(processed.scaled.train$Y, ncol=1)
y.test <- as.matrix(processed.scaled.test$Y, ncol=1)

# create 45-45-10 sample
smplcausal <- sample(nrow(processed.scaled.train), 
                     round(5*nrow(processed.scaled.train)/10), replace=FALSE)
processed.scaled.train.1 <- processed.scaled.train[smplcausal,]
processed.scaled.train.2 <- processed.scaled.train[-smplcausal,]

########################
#        Remark        #
########################
# When we want to scale the variables with the presence of training, validation, and test sets, 
# we usually use only the training set to obtain the mean and sd. Then we scale all three sets
# using the obtained mean and sd. 
#################### ############### ############# ############## # # #

# Creating Formulas
# For many of the models, we will need a "formula"
# This will be in the format Y ~ X1 + X2 + X3 + ...
# For more info, see: http://faculty.chicagobooth.edu/richard.hahn/teaching/formulanotation.pdf
print(covariate.names)
sumx <- paste(covariate.names, collapse = " + ")  # "X1 + X2 + X3 + ..." for substitution later
interx <- paste(" (",sumx, ")^2", sep="")  # "(X1 + X2 + X3 + ...)^2" for substitution later

# Y ~ X1 + X2 + X3 + ... 
linearnotreat <- paste("Y",sumx, sep=" ~ ")
linearnotreat <- as.formula(linearnotreat)
linearnotreat

# Y ~ W + X1 + X2 + X3 + ...
linear <- paste("Y",paste("W",sumx, sep=" + "), sep=" ~ ")
linear <- as.formula(linear)
linear

# Y ~ W * (X1 + X2 + X3 + ...)   
# ---> X*Z means include these variables plus the interactions between them
linearhet <- paste("Y", paste("W * (", sumx, ") ", sep=""), sep=" ~ ")
linearhet <- as.formula(linearhet)
linearhet


###### # ######### # ############## # ################### # ##################
# We can now use these formulas to do linear regression and logit regression #
###### # ######### # ############## # ################### # ##################

#####################
# Linear Regression #
#####################
lm.linear <- lm(linear, data=processed.scaled)
summary(lm.linear)

lm.linearhet <- lm(linearhet, data=processed.scaled)
summary(lm.linearhet)

#######################
# Logistic Regression #
#######################
# See:http://www.ats.ucla.edu/stat/r/dae/logit.htm

# The code below estimates a logistic regression model using 
# the glm (generalized linear model) function. 
mylogit <- glm(linear, data = processed.scaled, family = "binomial")
summary(mylogit)

##################################
# LASSO variable selection + OLS #
##################################
# see https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
# and also help(glmnet)

# LASSO takes in a model.matrix
# First parameter is the model (here we use linear, which we created before)
# Second parameter is the dataframe we want to creaate the matrix from
linear.train <- model.matrix(linear, processed.scaled.train)[,-1]
linear.test <- model.matrix(linear, processed.scaled.test)[,-1]
linear.train.1 <- model.matrix(linear, processed.scaled.train.1)[,-1]
linear.train.2 <- model.matrix(linear, processed.scaled.train.2)[,-1]

# Use cross validation to select the optimal shrinkage parameter lambda
# and the non-zero coefficients
lasso.linear <- cv.glmnet(linear.train.1, y.train[smplcausal,],  alpha=1, parallel=TRUE)

# prints the model, somewhat information overload, 
# but you can see the mse, and the nonzero variables and the cross validation steps
lasso.linear

# plot & select the optimal shrinkage parameter lambda
# Note: if you see an error message "figure margins too large",
# try expanding the plot area in RStudio.
plot(lasso.linear)
lasso.linear$lambda.min
lasso.linear$lambda.1se
# lambda.min gives min average cross-validated error 
# lambda.1se gives the most regularized model such that error is 
# within one standard error of the min; this value of lambda is used here.

# List non-zero coefficients found. There are two ways to do this.
# coef(lasso.linear, s = lasso.linear$lambda.1se) # Method 1
coef <- predict(lasso.linear, type = "nonzero") # Method 2

# index the column names of the matrix in order to index the selected variables
colnames <- colnames(linear.train.1)
selected.vars <- colnames[unlist(coef)]

# do OLS using these coefficients
linearwithlass <- paste("Y", paste(append(selected.vars, "W"),collapse=" + "), sep = " ~ ") 
linearwithlass <- as.formula(linearwithlass)
lm.linear.lasso <- lm(linearwithlass, data=processed.scaled.train.2)
yhat.linear.lasso <- predict(lm.linear.lasso, newdata=processed.scaled.test)
summary(lm.linear.lasso)

#################################
# Lasso for Logistic Regression #
#################################
lasso.logit <- glmnet(linear.train.1, y.train[smplcausal,], alpha=1, family='binomial')

# visualize the coefficient paths
plot(lasso.logit, label = FALSE)
grid()
# Each variable has one curve, which is the path of its coefficient as
# the shrinkage parameter lambda varies. 
# The axis tells the number of nonzero coefficients at the current 
# value of lambda, which is the effective degrees of freedom (Df) for the lasso. 
# You can set label = TRUE in the plot command to annotate the curves. 

# We can obtain the coefficients at one or more values of lambda 
coef(lasso.logit, s = c(0.03457, 0.007))

# predict with particular values of lambda (use variable s)
pre.lasso.logit <- predict(lasso.logit, newx = as.matrix(processed.scaled.test[, -1]), 
                   s = c(0.03457, 0.007), type = "response")
# If we don't include type = "response" in the predict() formula, 
# it will return the values of log(Y/(1-Y))

### use cross validation to choose the optimal lambda
cv.glm.logit <- cv.glmnet(linear.train.1, y.train[smplcausal,], 
                           alpha=1, parallel=TRUE)
plot(cv.glm.logit)
opt.lambda <- cv.glm.logit$lambda.1se 
# Some people use lambda.min but it seems that lambda.1se is preferred. 

# see the coefficients with the optimally chosen lambda
coef(cv.glm.logit, s = opt.lambda)

#### # Remark # ####
# The results of cv.glmnet are random, since the folds are selected at random.
# If cv.glmnet always uses the same set of covariates (say all covariate), then
# the results will be stable. Or if we use LOOCV, the results will also be stable.
# Otherwise, they will vary when we run the function multiple times.
################ ##################### # #########################

# prediction using cv.glmnet is therefore not stable in general 
pre.cv.glm.logit <- predict(cv.glm.logit, newx = as.matrix(processed.scaled.test[, -1]), 
                s = opt.lambda)

# It will be more stable to do prediction using glmnet with the optimal lambda
# using type = "class" gives the classified category
pre.opt.lambda.lasso.logit <- predict(lasso.logit, 
                                       newx = as.matrix(processed.scaled.test[, -1]), 
                                       s = opt.lambda, type = "class")

#######################################
# Elastic net for Logistic Regression #
#######################################

# This is almost the same as the previous part, except that we use alpha in (0, 1)
elastNet.logit <- glmnet(linear.train.1, y.train[smplcausal,], 
                         alpha = 0.2, family = 'binomial')

# plot the coefficient paths against the log-lambda values
plot(elastNet.logit, xvar = "lambda", label = FALSE)
grid()

# We can also use the same folds so we can select the optimal value for alpha.
# Above, we set alpha = 0.2
# Use foldid: a vector of values between 1 and nfold identifying what fold 
# each observation is in.
foldid <- sample(1:10, size = length(y.train[smplcausal,]), replace = TRUE)
cv0.2.elastNet.logit <- cv.glmnet(linear.train.1, y.train[smplcausal,], 
                                  foldid = foldid, alpha = 0.2, parallel=TRUE)
cv0.5.elastNet.logit <- cv.glmnet(linear.train.1, y.train[smplcausal,], 
                                  foldid = foldid, alpha = 0.5, parallel=TRUE)
cv0.8.elastNet.logit <- cv.glmnet(linear.train.1, y.train[smplcausal,], 
                                  foldid = foldid, alpha = 0.8, parallel=TRUE)

# plot all three MSE's in the same plot to compare
# par(mfrow = c(2,2))
# plot(cv0.2.elastNet.logit); plot(cv0.5.elastNet.logit); plot(cv0.8.elastNet.logit)
plot(log(cv0.8.elastNet.logit$lambda), cv0.8.elastNet.logit$cvm, pch = 19, 
     col = "red", xlab = "log(Lambda)", ylab = cv0.2.elastNet.logit$name)
points(log(cv0.5.elastNet.logit$lambda), cv0.5.elastNet.logit$cvm, pch=19, col="grey")
points(log(cv0.2.elastNet.logit$lambda), cv0.2.elastNet.logit$cvm, pch=19, col="blue")
legend("topleft",legend = c("alpha = 0.8", "alpha = 0.5", "alpha = 0.2"),
       pch = 19, col = c("red","grey","blue"))

# We can plot with more values of alpha to choose the best alpha. According to the plot,
# it seems like alpha = 0.8 does the best among the three reported values. 

# Using this alpha, we can find the optimal lambda and coefs as in the previous part. 
opt0.8.lambda <- cv0.8.elastNet.logit$lambda.1se
coef(cv0.8.elastNet.logit, s = opt0.8.lambda)

###################################
# Single Tree uses linear formula #
###################################

# Classification Tree with rpart
# grow tree 
set.seed(444)
linear.singletree <- rpart(formula = linear, data=processed.scaled.train, 
                         method = "anova", y=TRUE,
                         control=rpart.control(cp=1e-04, minsplit=30))

linear.singletree$cptable
printcp(linear.singletree) # display the results 
plotcp(linear.singletree) # visualize cross-validation results 

# very detailed summary of splits, uncomment the code below and execute to see
# summary(linear.singletree) 

# prune the tree
op.index <- which.min(linear.singletree$cptable[, "xerror"])
cp.vals <- linear.singletree$cptable[, "CP"]
treepruned.linearsingle <- prune(linear.singletree, cp = cp.vals[op.index])

### Remark ###
# Some people prefer using 1 SE rule to plot a decent tree - 
# find the cp corresponding to the largest xerror within 1 SE of the 
# xerror corresponding to the min cp 
####### # ####### #############

# apply model to the test set to get predictions
singletree.pred.class <- predict(treepruned.linearsingle, newdata=processed.scaled.test)

# plot tree 
plot(treepruned.linearsingle, uniform=TRUE, 
     main="Classification Tree Example")
text(treepruned.linearsingle, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree  
# (still not super attractive, saves it in current directory)
post(treepruned.linearsingle, file = "tree.ps", 
     title = "Classification Tree Example")

# Visualize (the first few layers of) the tree 
# We would need to adjust the complexity parameter cp
visual.pruned.tree <- prune(linear.singletree, cp = 0.003)
plot(visual.pruned.tree, uniform=TRUE, 
     main="Visualize The First Few Layers of The Tree")
text(visual.pruned.tree, use.n=TRUE, all=TRUE, cex=.8)

post(visual.pruned.tree, file = "visual_tree.ps", 
     title = "Visualize The First Few Layers of The Tree")

#################################
# Random Forest (Single Forest) # 
#################################
# use linear formula

# We first set the outcome variable as factor to indicate that
# we want to do classification with random forest
processed.scaled.train$Y <- as.factor(processed.scaled.train$Y)

# fitting model
set.seed(90)
fit.rf <- randomForest(linear, processed.scaled.train, ntree = 50, do.trace = 10)

summary(fit.rf)
print(fit.rf)
imp.fit.rf <- randomForest::importance(fit.rf)

# plotting
plot(fit.rf)
plot(imp.fit.rf, lty = 2, pch = 16)
lines(imp.fit.rf)

# create partial dependence plots
imp <- imp.fit.rf
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 2))
for (i in seq_along(impvar)) {
  partialPlot(fit.rf, processed.scaled.train, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(0, 1))
}

# Predict Output 
predicted <- predict(fit.rf, processed.scaled.test)

par(mfrow=c(1,1))
varImpPlot(fit.rf)

