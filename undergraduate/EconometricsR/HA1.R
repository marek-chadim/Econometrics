my_log <- file("HA1_log.txt")

sink(my_log, append = TRUE, type = "output") # Writing console output to log file
sink(my_log, append = TRUE, type = "message")

library(readr)
library(tseries)
library(tseries)
library(nlme)
library(lmtest)
library(sandwich)
library(robustbase)
library(prais) 

#METHODOLOGY AND DATA 

#estimation of Y=f (x_k) by OLS

#Dependent Variable Y - Consumer Price Index Growth Rate, Not Seasonally Adjusted
inf <- ts(data=read_csv("CPI.csv")[,2], start = c(2000, 1),end = c(2022, 7), frequency = 12)
par(mfrow=c(2,2))
ts.plot(inf, xlab = "Year", ylab = "Growth rate same period previous year", main = "Consumer Price Index: Total All Items for the United States", type ="b")
boxplot(inf)
acf(inf)
pacf(inf, main = " ")


#Explanatory Variable X1 - Crude Oil Price, Not Seasonally Adjusted
op <- ts(data=read_csv("WTI.csv")[,2], start = c(2000, 1),end = c(2022, 7), frequency = 12)
ts.plot(op, xlab = "Year", ylab = "Dollars per Barrel", main = "Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma", type ="b")
boxplot(op)
acf(inf, main = " ")
pacf(inf, main = " ")

#Additional Explanatory Variables

#X2 - Unemployment rate (%)
unr <- ts(data=read_csv("UNRATE.csv")[,2], start = c(2000, 1),end = c(2022, 7), frequency = 12)

#X3 GDP growth (%)
gdpg <- ts(data=read_csv("GDPG.csv")[,2], start = c(2000, 1),end = c(2022, 7), frequency = 12)

#X4 – Exports (USD)
ex <- ts(data=read_csv("EX.csv")[,2], start = c(2000, 1),end = c(2022, 7), frequency = 12)

#X5 - Imports (USD)
im <- ts(data=read_csv("IM.csv")[,2], start = c(2000, 1),end = c(2022, 7), frequency = 12)

#X6 – Money supply M3 (USD)
m3 <- ts(data=read_csv("M3.csv")[,2], start = c(2000, 1),end = c(2022, 7), frequency = 12)

#X7 - Nominal exchange rate (%)
fxr <- ts(data=read_csv("FXR.csv")[,2], start = c(2000, 1),end = c(2022, 7), frequency = 12)

#X8 - Short term Interest rate (%)
intr <- ts(data= read_csv("INTR.csv")[,2], start = c(2000, 1),end = c(2022, 7), frequency = 12)

data<- data.frame(inf,op,unr,gdpg,ex,im,m3,fxr,intr)
#EMPIRICAL RESULTS

#Testing for Stationarity
lagpad <- function(x, k) {
  i<-length(x)
  j=i-k
  x<-as.vector(c(rep(NA,k),x[1:j]))
}
summary(lm(op ~lagpad(op, 1)))
adf.test(op)
d_op<-diff(op,1)
adf.test(d_op) #I(1)

summary(lm(inf ~lagpad(inf, 1)))
adf.test(inf)
d_inf<-diff(inf,1)
adf.test(d_inf) #I(1)

par(mfrow=c(1,2))
ts.plot(d_op,  ylab = "d_op", type ="b")
ts.plot(d_op, ylab = "d_inf",  type ="b")

#adf.test(x_k), k=2,...7
#only gdpg I(0), rest is I(1)
summary(lm(d_inf ~ d_op + diff(unr,1) +gdpg[-1] + diff(ex,1) + diff(im,1) + diff(m3, 1) + diff(fxr,1) + diff(intr,1)))


#Static model vs FDL model
summary(lm(d_inf ~ d_op))
summary(lm(d_inf ~d_op + lagpad(d_op,6) +lagpad(d_op,12)+lagpad(d_op,18)))

#self-fullfiling expectations?
summary(lm(d_inf ~  +lagpad(d_inf,12)+lagpad(d_inf,24)+lagpad(d_inf,36)))


model<-lm(d_inf ~ d_op + lagpad(d_op,6) +lagpad(d_op,12)+lagpad(d_op,18)
                 +gdpg[-1] + diff(ex,1) + diff(im,1) + diff(fxr,1) + diff(intr,1)
                 +lagpad(gdpg[-1],12)+ lagpad(diff(m3,1),12)+ lagpad(diff(unr,1),12)
                 +lagpad(d_inf,12)+lagpad(d_inf,24)+lagpad(d_inf,36))
          
summary(model)

# Serial correlation in residuals
par(mfrow=c(1,2))
res<-residuals(model)
acf(res)
pacf(res)
res_1 = lagpad(res, 1)
res_2 = lagpad(res_1, 1)
res_3 = lagpad(res_2, 1)
res_4 = lagpad(res_3, 1)
summary(lm(res ~ res_1 + res_2 + res_3 + res_4  - 1)) #AR(3) at least


#also
bgtest(model, order = 5)
#BG test H_0: No serial correlation. We reject null hypothesis.

library(robustbase)
#?lmrob()
model_robust<-lmrob(d_inf ~ d_op + lagpad(d_op,6) +lagpad(d_op,12)+lagpad(d_op,18)
          +gdpg[-1] + diff(ex,1) + diff(im,1) + diff(fxr,1) + diff(intr,1)
          +lagpad(gdpg[-1],12)+ lagpad(diff(m3,1),12)+ lagpad(diff(unr,1),12)
          +lagpad(d_inf,12)+lagpad(d_inf,24)+lagpad(d_inf,36),
          cov = ".vcov.w")

summary(model_robust)

#alternative procedures, would need to manually reduce observations to the number of residuals (234)

lres <- log(residuals(model)^2)
#gls_res <- lm(lres ~ d_op + lagpad(d_op,6) +lagpad(d_op,12)+lagpad(d_op,18)
 #             +gdpg[-1] + diff(ex,1) + diff(im,1) + diff(fxr,1) + diff(intr,1)
  #            +lagpad(gdpg[-1],12)+ lagpad(diff(m3,1),12)+ lagpad(diff(unr,1),12)
   #           +lagpad(d_inf,12)+lagpad(d_inf,24)+lagpad(d_inf,36))

#hhat <- exp(gls_res$fitted.values)
#gls <- lm(lres ~ d_op + lagpad(d_op,6) +lagpad(d_op,12)+lagpad(d_op,18)
 #         +gdpg[-1] + diff(ex,1) + diff(im,1) + diff(fxr,1) + diff(intr,1)
  #        +lagpad(gdpg[-1],12)+ lagpad(diff(m3,1),12)+ lagpad(diff(unr,1),12)
   #       +lagpad(d_inf,12)+lagpad(d_inf,24)+lagpad(d_inf,36))

#model_fg<-gls(d_inf ~ d_op + lagpad(d_op,6) +lagpad(d_op,12)+lagpad(d_op,18)
 #                   +gdpg[-1] + diff(ex,1) + diff(im,1) + diff(fxr,1) + diff(intr,1)
  #                  +lagpad(gdpg[-1],12)+ lagpad(diff(m3,1),12)+ lagpad(diff(unr,1),12)
   #                 +lagpad(d_inf,12)+lagpad(d_inf,24)+lagpad(d_inf,36),
    #          
     #        correlation = corARMA(p = 3, q = 0))

# 1. saving the estimated autoregressive coefficient of AR(1) model in residuals
# 2. Quasi-differencing of dependent and independent variables
# 3. Running the regression of quasi-differenced variables

#library(prais) and prais_winsten()


# Heteroskedasticity
#The White test that regresseses squared residuals on explanatory variables, their squares and cross-products.
#H0 : all parameters = 0 (i.e., homoskedasticity). 

white<- lm(res^2~d_op[1:234]+I(d_op[1:234]^2),data=data) #the case of one regressor
summary(white)
f_statistics=summary(white)$fstatistic
pf(f_statistics[1],f_statistics[2],f_statistics[3], lower.tail = F)


#Construction of SC (and heteroskedasticity) robust SE
coeftest(model, vcov = vcovHAC(model))

#Dummy variables

data<- data.frame(inf,op,unr,gdpg,ex,im,m3,fxr,intr)
data<-as.ts(data)
data <- ts(data=data, start = 2000, frequency = 12)
data <- data.frame(data,date=time(data))
names(data)[names(data) == 'CPALTT01USM659N'] <- 'inf'
names(data)[names(data) == 'MABMM301USM189S'] <- 'm3'
data <- cbind(data, dummy_22=as.numeric(data$date>=2022))
data <- cbind(data, dummy_7=as.numeric(data$date>=2007&data$date<=2008))
dummy_22<- ts(data$dummy_22)
dummy_7<-ts(data$dummy_7)

model_dummy<-lmrob(d_inf ~ d_op + lagpad(d_op,6) +lagpad(d_op,12)+lagpad(d_op,18)
                    +gdpg[-1] + diff(ex,1) + diff(im,1) + diff(fxr,1) + diff(intr,1)
                    +lagpad(gdpg[-1],12)+ lagpad(diff(m3,1),12)+ lagpad(diff(unr,1),12)
                    +lagpad(d_inf,12)+lagpad(d_inf,24)+lagpad(d_inf,36)
                    +dummy_7[-1] +dummy_22[-1],
                     cov = ".vcov.w")
summary(model_dummy)

