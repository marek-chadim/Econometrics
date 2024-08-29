install.packages(c("rpart","randomForest"))

##Bagging

#Preparing Spambase data and evaluating the performance of decision trees

#Load the data, split into training and testing sample in 90:10 ratio.
spamD<-read.table('spamD.tsv',header=T,sep='\t')
spamTrain<-subset(spamD,spamD$rgroup>=10)
spamTest<-subset(spamD,spamD$rgroup<10)

spamVars<-setdiff(colnames(spamD),list('rgroup','spam')) #Specify the independent variables.
spamFormula<-as.formula(paste('spam=="spam"', #Construct the formula (set "spam" as a "yes").
                                  paste(spamVars,collapse=' + '),sep=' ~ '))

loglikelihood<-function(y,py) { #Likelihood is later needed for deviance (y==truth,py==pred).
  pysmooth<-ifelse(py==0,1e-12, #Smoothing is not completely necessary but it surely does not hurt.
              ifelse(py==1,1-1e-12,py))
  sum(y*log(pysmooth)+(1-y)*log(1-pysmooth))
}
accuracyMeasures<-function(pred,truth,name="model") { #Function for various accuracy measures.
  dev.norm <- -2*loglikelihood(as.numeric(truth),pred)/length(pred) #Normalized deviance so that we can better compare across datasets
  ctable<-table(truth=truth,
                  pred=(pred>0.5)) #Set a threshold of 0.5
  accuracy<-sum(diag(ctable))/sum(ctable)
  precision<-ctable[2,2]/sum(ctable[,2])
  recall<-ctable[2,2]/sum(ctable[2,])
  f1<-2*precision*recall/(precision+recall)
  data.frame(model=name, accuracy=accuracy, f1=f1, dev.norm)
}

#Evaluate the decision tree model against the training and test sets.
library(rpart)
treemodel<-rpart(spamFormula,spamTrain)
#plot(treemodel)
accuracyMeasures(predict(treemodel, newdata=spamTrain),   
                   spamTrain$spam=="spam",
                   name="tree, training")
accuracyMeasures(predict(treemodel, newdata=spamTest),
                 spamTest$spam=="spam",
                 name="tree, test")

#Compare with a simple logit model
logitmodel<-glm(spamFormula,family=binomial(link="logit"),data=spamTrain)
accuracyMeasures(predict(logitmodel, newdata=spamTrain,type="response"),   
                 spamTrain$spam=="spam",
                 name="logit, training")
accuracyMeasures(predict(logitmodel, newdata=spamTest,type="response"),   
                 spamTest$spam=="spam",
                 name="logit, test")

#Bagging decision trees 

#Use bootstrap samples the same size as the training set, with 33 trees.
n<-dim(spamTrain)[1]
ntree<-33
#Build the bootstrap samples by sampling the row indices of spamTrain with replacement. 
#Each column of the matrix samples represents the row indices into spamTrain 
#that comprise the bootstrap sample. 
samples<-sapply(1:ntree,
            FUN = function(iter)
            {sample(1:n, size=n, replace=T)}) #replace=T makes it a bootstrap (in its very basic form)
head(samples)
hist(samples[,1])
#Train the individual decision trees and return them 
#in a list. Note: this step can take a few minutes.
treelist<-lapply(1:ntree,
            FUN=function(iter) {
              samp <- samples[,iter];
              rpart(spamFormula,spamTrain[samp,])
              }
            )
class(treelist)
treelist[[1]]
plot(treelist[[1]])
plot(treelist[[33]])
#predict.bag assumes the underlying classifier returns decision probabilities, not decisions. 
predict.bag<-function(treelist,newdata) {
  preds<-sapply(1:length(treelist),
                FUN=function(iter) {
                predict(treelist[[iter]],newdata=newdata)
                  }
                )
  predsums<-rowSums(preds)
  predsums/length(treelist)
}
#Evaluate the bagged decision trees against the training and test sets.
accuracyMeasures(predict.bag(treelist, newdata=spamTrain),
                   spamTrain$spam=="spam",
                   name="bagging, training")
accuracyMeasures(predict.bag(treelist, newdata=spamTest),
                   spamTest$spam=="spam",
                   name="bagging, test")

##Random forests

#Using random forests 

library(randomForest)
set.seed(5123512)
fmodel<-randomForest(x=spamTrain[,spamVars], #Independet variables as x
                         y=as.factor(spamTrain$spam), #Depependet variable
                         ntree=33,     #Use the same number of trees as for the bagging procedure.
                         nodesize=7,    #Specify that each node of a tree must have a minimum of 7 elements.
                         importance=T)  #We will need this later for the important variable search/identification.
?randomForest
accuracyMeasures(predict(fmodel,
                    newdata=spamTrain[,spamVars],type='prob')[,'spam'],
                    spamTrain$spam=="spam",name="random forest, train")
accuracyMeasures(predict(fmodel,
                    newdata=spamTest[,spamVars],type='prob')[,'spam'],
                    spamTest$spam=="spam",name="random forest, test")

#randomForest variable importances 
varImp<-importance(fmodel)
varImp[1:10, ]
varImpPlot(fmodel, type=1)

#Fitting with fewer variables

#Sort the variables by their importance, as measured by accuracy change (and pick the first 25), estimate and compare the models.
selVars<-names(sort(varImp[,1],decreasing=T))[1:25]
fsel<-randomForest(x=spamTrain[,selVars],
                   y=as.factor(spamTrain$spam),
                       ntree=33,
                       nodesize=7,
                       importance=T)
accuracyMeasures(predict(fsel,
                    newdata=spamTrain[,selVars],type='prob')[,'spam'],
                    spamTrain$spam=="spam",name="RF small, train")
accuracyMeasures(predict(fsel,
                    newdata=spamTest[,selVars],type='prob')[,'spam'],
                    spamTest$spam=="spam",name="RF small, test")
