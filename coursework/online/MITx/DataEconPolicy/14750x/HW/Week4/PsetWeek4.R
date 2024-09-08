############################################################################################################################/
##################### Problem Set: Week 4 ###############################################################################
############################################################################################################################/
# 14.750x: Political Economy and Economic Development
############################################################################################################################/

#**************************************************************************************************************************/
# 0. Initial adjustment and necessary package loading ----------------------------------------------------------------
#**************************************************************************************************************************/

options(scipen=999)
options(max.print = 99999999)
rm(list=(ls()))

library(rio)
library(dplyr)
library(tidyr)
library(ggplot2)
library(AER)
library(ivreg)
library(multiwayvcov)

getwd()
data <- import("mitaData.rdata")


#*****************************************************************************/
# 2.a  ----------------------------------------------------------------
#*****************************************************************************/

data$x_2<-data$x*data$x
data$y_2<-data$y*data$y
data$x_y<-data$x*data$y
data$x_3<-data$x*data$x*data$x
data$y_3<-data$y*data$y*data$y
data$x_2_y<-data$x*data$x*data$y
data$y_2_x<-data$y*data$y*data$x

mean(data$x_2)
mean(data$y_3)
mean(data$y_2_x)


#*****************************************************************************/
# 2.b  ----------------------------------------------------------------
#*****************************************************************************/

formula = "lhhequiv~pothuan_mita+elv_sh+slope+infants+children+adults+bfe4_1+
bfe4_2+bfe4_3+x+y+x_2+y_2+x_y+x_3+y_3+x_2_y+y_2_x"

data100<-data[data$d_bnd<100,]
data75<-data[data$d_bnd<75,]
data50<-data[data$d_bnd<50,]


lm100 <-lm(formula, data=data100)
district100 <- data100$district
ses100 <- cluster.vcov(lm100, district100)

coeftest(lm100, ses100)


lm75 <-lm(formula, data=data75)
district75 <- data75$district
ses75 <- cluster.vcov(lm75, district75)

coeftest(lm75, ses75)


lm50 <-lm(formula, data=data50)
district50 <- data50$district
ses50 <- cluster.vcov(lm50, district50)

coeftest(lm50, ses50)



#*****************************************************************************/
# 2.c  ----------------------------------------------------------------
#*****************************************************************************/

data$dpot_2<-data$dpot*data$dpot
data$dpot_3<-data$dpot*data$dpot*data$dpot

data100<-data[data$d_bnd<100,]
data75<-data[data$d_bnd<75,]
data50<-data[data$d_bnd<50,]


formula_dpot = "lhhequiv~pothuan_mita+elv_sh+slope+infants+children+adults+bfe4_1+
bfe4_2+bfe4_3+dpot+dpot_2+dpot_3"

lm100_dpot <-lm(formula_dpot, data=data100)
ses100 <- cluster.vcov(lm100_dpot, district100)

coeftest(lm100_dpot, ses100)


lm75_dpot <-lm(formula_dpot, data=data75)
ses75 <- cluster.vcov(lm75_dpot, district75)

coeftest(lm75_dpot, ses75)


lm50_dpot <-lm(formula_dpot, data=data50)
ses50 <- cluster.vcov(lm50_dpot, district50)

coeftest(lm50_dpot, ses50)




