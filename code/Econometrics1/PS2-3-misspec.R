setwd("./Figures")
set.seed(112087)

Z <- 1000
nvec <- c(50,100,250,1000)
tilde <- c(1/2,1,2,3)
lambda = 1

ests <- array(NA,dim=c(length(nvec),Z,4,4))
# Dimensions: 
#  1: Sample size
#  2: Iteration
#  3: x tilde
#  4: Estimator
res <- array(NA,dim=c(length(nvec),4,4,3))
# Dimensions: 
#  1: Sample size
#  2: x tilde
#  3: Estimator
#  4: Bias, variance, MSE

for (ncount in 1:length(nvec)) {
  n <- rexp(nvec[ncount])
  for (z in 1:Z) {
    x <- rexp(n,rate=lambda)
    muhat <- mean(x)
    sighat <- sd(x)
    lamhat <- 1/muhat
    lamhat2 <- (1/2)*1/muhat + (1/2)*1/sighat
    for (xtcount in 1:length(tilde)) {
	xt <- tilde[xtcount]
	ests[ncount,z,xtcount,1] <- pexp(xt,rate=lamhat)
	ests[ncount,z,xtcount,2] <- pexp(xt,rate=lamhat2)
	ests[ncount,z,xtcount,3] <- pnorm(xt,mean=muhat,sd=sighat)
	ests[ncount,z,xtcount,4] <- mean(x < xt)
    }
  }
}

for (ncount in 1:length(nvec)) {
  for (xtcount in 1:length(tilde)) {
    xt <- tilde[xtcount]
    for(i in 1:4) {
      res[ncount,xtcount,i,1] <- mean(ests[ncount,,xtcount,i]) - pexp(xt,rate=lambda)
      res[ncount,xtcount,i,2] <- var(ests[ncount,,xtcount,i])
      res[ncount,xtcount,i,3] <- res[ncount,xtcount,i,1]^2 + res[ncount,xtcount,i,2]
    }
  }
}


setEPS()

XL <- 700


postscript("PS2-3-1.eps")
plot(nvec,res[,1,1,1],type="l",col="green",xlim=c(0,max(Nlist)),ylim=c(min(res),max(res)),xlab="Sample size", ylab="Bias (xtilde = 1/2)")
lines(nvec,res[,1,2,1],type="l",col="blue")
lines(nvec,res[,1,3,1],type="l",col="purple")
lines(nvec,res[,1,4,1],type="l",col="red")
legend(XL,-.02,legend=c("exp, MLE","exp, lambda-2","norm, MLE","Empirical"),fill=c("green","blue","purple","red"))
dev.off()

postscript("PS2-3-2.eps")
plot(nvec,res[,2,1,1],type="l",col="green",xlim=c(0,max(Nlist)),ylim=c(min(res),max(res)),xlab="Sample size", ylab="Bias (xtilde = 1)")
lines(nvec,res[,2,2,1],type="l",col="blue")
lines(nvec,res[,2,3,1],type="l",col="purple")
lines(nvec,res[,2,4,1],type="l",col="red")
legend(XL,-.02,legend=c("exp, MLE","exp, lambda-2","norm, MLE","Empirical"),fill=c("green","blue","purple","red"))
dev.off()

postscript("PS2-3-3.eps")
plot(nvec,res[,3,1,1],type="l",col="green",xlim=c(0,max(Nlist)),ylim=c(min(res),max(res)),xlab="Sample size", ylab="Bias (xtilde = 2)")
lines(nvec,res[,3,2,1],type="l",col="blue")
lines(nvec,res[,3,3,1],type="l",col="purple")
lines(nvec,res[,3,4,1],type="l",col="red")
legend(XL,-.02,legend=c("exp, MLE","exp, lambda-2","norm, MLE","Empirical"),fill=c("green","blue","purple","red"))
dev.off()

postscript("PS2-3-4.eps")
plot(nvec,res[,4,1,1],type="l",col="green",xlim=c(0,max(Nlist)),ylim=c(min(res),max(res)),xlab="Sample size", ylab="Bias (xtilde = 3)")
lines(nvec,res[,4,2,1],type="l",col="blue")
lines(nvec,res[,4,3,1],type="l",col="purple")
lines(nvec,res[,4,4,1],type="l",col="red")
legend(XL,-.02,legend=c("exp, MLE","exp, lambda-2","norm, MLE","Empirical"),fill=c("green","blue","purple","red"))
dev.off()




postscript("PS2-3-5.eps")
plot(nvec,res[,1,1,2],type="l",col="green",xlim=c(0,max(Nlist)),ylim=c(0,.007),xlab="Sample size", ylab="Variance (xtilde = 1/2)")
lines(nvec,res[,1,2,2],type="l",col="blue")
lines(nvec,res[,1,3,2],type="l",col="purple")
lines(nvec,res[,1,4,2],type="l",col="red")
legend(XL,.007,legend=c("exp, MLE","exp, lambda-2","norm, MLE","Empirical"),fill=c("green","blue","purple","red"))
dev.off()

postscript("PS2-3-6.eps")
plot(nvec,res[,2,1,2],type="l",col="green",xlim=c(0,max(Nlist)),ylim=c(0,.007),xlab="Sample size", ylab="Variance (xtilde = 1)")
lines(nvec,res[,2,2,2],type="l",col="blue")
lines(nvec,res[,2,3,2],type="l",col="purple")
lines(nvec,res[,2,4,2],type="l",col="red")
legend(XL,.007,legend=c("exp, MLE","exp, lambda-2","norm, MLE","Empirical"),fill=c("green","blue","purple","red"))
dev.off()

postscript("PS2-3-7.eps")
plot(nvec,res[,3,1,2],type="l",col="green",xlim=c(0,max(Nlist)),ylim=c(0,.007),xlab="Sample size", ylab="Variance (xtilde = 2)")
lines(nvec,res[,3,2,2],type="l",col="blue")
lines(nvec,res[,3,3,2],type="l",col="purple")
lines(nvec,res[,3,4,2],type="l",col="red")
legend(XL,.007,legend=c("exp, MLE","exp, lambda-2","norm, MLE","Empirical"),fill=c("green","blue","purple","red"))
dev.off()

postscript("PS2-3-8.eps")
plot(nvec,res[,4,1,2],type="l",col="green",xlim=c(0,max(Nlist)),ylim=c(0,.007),xlab="Sample size", ylab="Variance (xtilde = 3)")
lines(nvec,res[,4,2,2],type="l",col="blue")
lines(nvec,res[,4,3,2],type="l",col="purple")
lines(nvec,res[,4,4,2],type="l",col="red")
legend(XL,.007,legend=c("exp, MLE","exp, lambda-2","norm, MLE","Empirical"),fill=c("green","blue","purple","red"))
dev.off()



postscript("PS2-3-9.eps")
plot(nvec,res[,1,1,3],type="l",col="green",xlim=c(0,max(Nlist)),ylim=c(0,.02),xlab="Sample size", ylab="MSE (xtilde = 1/2)")
lines(nvec,res[,1,2,3],type="l",col="blue")
lines(nvec,res[,1,3,3],type="l",col="purple")
lines(nvec,res[,1,4,3],type="l",col="red")
legend(XL,.02,legend=c("exp, MLE","exp, lambda-2","norm, MLE","Empirical"),fill=c("green","blue","purple","red"))
dev.off()

postscript("PS2-3-10.eps")
plot(nvec,res[,2,1,3],type="l",col="green",xlim=c(0,max(Nlist)),ylim=c(0,.02),xlab="Sample size", ylab="MSE (xtilde = 1)")
lines(nvec,res[,2,2,3],type="l",col="blue")
lines(nvec,res[,2,3,3],type="l",col="purple")
lines(nvec,res[,2,4,3],type="l",col="red")
legend(XL,.02,legend=c("exp, MLE","exp, lambda-2","norm, MLE","Empirical"),fill=c("green","blue","purple","red"))
dev.off()

postscript("PS2-3-11.eps")
plot(nvec,res[,3,1,3],type="l",col="green",xlim=c(0,max(Nlist)),ylim=c(0,.02),xlab="Sample size", ylab="MSE (xtilde = 2)")
lines(nvec,res[,3,2,3],type="l",col="blue")
lines(nvec,res[,3,3,3],type="l",col="purple")
lines(nvec,res[,3,4,3],type="l",col="red")
legend(XL,.02,legend=c("exp, MLE","exp, lambda-2","norm, MLE","Empirical"),fill=c("green","blue","purple","red"))
dev.off()

postscript("PS2-3-12.eps")
plot(nvec,res[,4,1,3],type="l",col="green",xlim=c(0,max(Nlist)),ylim=c(0,.02),xlab="Sample size", ylab="MSE (xtilde = 3)")
lines(nvec,res[,4,2,3],type="l",col="blue")
lines(nvec,res[,4,3,3],type="l",col="purple")
lines(nvec,res[,4,4,3],type="l",col="red")
legend(XL,.02,legend=c("exp, MLE","exp, lambda-2","norm, MLE","Empirical"),fill=c("green","blue","purple","red"))
dev.off()






#