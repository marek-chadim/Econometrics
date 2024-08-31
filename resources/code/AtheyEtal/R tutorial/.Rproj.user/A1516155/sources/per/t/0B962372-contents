### CODE FOR LECTURE #1 (not mandatory) ###
### JEB109 Econometrics I ###
### Institute of Economic Studies, Faculty of Social Sciences, Charles University ###

### PROBABILITY DISTRIBUTIONS ###

#Chi^2 distribution
df<-100 #number of standard Gaussians used
n<-10000 #number of observations
#Generate df-times Gaussian random variables with n observations
X<-sapply(1:df,FUN=function(iter){rnorm(n)})
ChiSq<-rowSums(X^2) #use the definition of the Chi^2 distribution
hist(ChiSq,breaks=33,freq=F)
#Compare with the actual Chi^2 distribution with given df
x_Chi<-seq(min(ChiSq),max(ChiSq),by=0.01)
y_Chi<-dchisq(x_Chi,df)
lines(x_Chi,y_Chi,lwd=2.0,col="red")

#Student's t distribution
df<-33 #df of the underlying Chi^2 distribution
n<-1000 #number of observations
t<-rnorm(n)/sqrt(rchisq(n,df)/df) #input the definition
hist(t,breaks=33,freq=F)
#Compare with the theoretic distribution
x_t<-seq(min(t),max(t),by=0.01)
y_t<-dt(x_t,df)
lines(x_t,y_t,lwd=2.0,col="red")

#Fisher-Snedecor F distribution
df1<-5 #df of the underlying Chi^2 distribution #1
df2<-100 #df of the underlying Chi^2 distribution #2
n<-10000 #number of observations
FF<-(rchisq(n,df1)/df1)/(rchisq(n,df2)/df2) #input the definition
hist(FF,breaks=33,freq=F)
#Compare with the theoretical distribution
x_F<-seq(min(FF),max(FF),by=0.01)
y_F<-df(x_F,df1,df2)
lines(x_F,y_F,lwd=2.0,col="red")

### LAW OF LARGE NUMBERS ###

#Let us demonstrate the (weak) LLN on a simple Gaussian distribution.

#We first set the parameters.
n<-1000 #number of random variables
mu<-1 #mean value
sigma2<-1 #variance
rep<-1000 #simulations repetitions

#Let us see the results for our repetitions.
XX<-numeric()
for(i in 1:rep){
  X<-rnorm(n,mu,sd=sqrt(sigma2))
  XX[i]<-abs(sum(X)/n-mu)  
}
plot(XX)

#Does the absolute value converge with an increasing n?
nn<-c(100,200,500,
      1000,2000,5000,
      10000,20000,50000
      #100000,200000,500000,
      #1000000
      )
convergence<-matrix(c(nn,NA*nn),ncol=2)
colnames(convergence)<-c("n","deviation")
for(j in nn){
  XX<-numeric()
  for(i in 1:rep){
    X<-rnorm(j,mu,sd=sqrt(sigma2))
    XX[i]<-abs(sum(X)/j-mu)  
  }
  convergence[convergence[,1]==j,2]<-mean(XX)
}
plot(convergence[,1],convergence[,2])
plot(log(convergence[,1],base=10),log(convergence[,2],base=10))
summary(lm(log(convergence[,2],base=10)~log(convergence[,1],base=10)))$coefficients[2,1]

### CENTRAL LIMIT THEOREM ###

#Let us demonstrate the CLT intuition on a rather extreme distribution: binomial.

#We first set the parameters.
trials<-1 #number of trials: just one trial leads to the most extreme distribution
p<-0.2 #probability of success
n<-100 #how many random variables do we draw
rep<-10000 #number of repetitions of the simulation

#Let us see the histogram for one run, compared to standard Gaussian.
X<-rbinom(n,trials,p)
hist(X,freq=F)
x_Gauss<-seq(min(X),max(X),by=0.01)
y_Gauss<-dnorm(x_Gauss,mean=0,sd=1)
lines(x_Gauss,y_Gauss,lwd=2.0,col="red") 

#Let us show the convergence in a simple FOR loop.
XX<-numeric()
for(i in 1:rep){
  X<-rbinom(n,trials,p)
  XX[i]<-(sum(X)/n-p)/(sqrt(p*(1-p))/sqrt(n))
}
hist(XX,freq=F)
x_Gauss<-seq(min(XX),max(XX),by=0.01)
y_Gauss<-dnorm(x_Gauss,mean=0,sd=1)
lines(x_Gauss,y_Gauss,lwd=2.0,col="red")