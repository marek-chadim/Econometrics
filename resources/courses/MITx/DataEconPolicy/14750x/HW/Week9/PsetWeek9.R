############################################################################################################################/
##################### Problem Set: Week 9 ###############################################################################
############################################################################################################################/
# 14.750x: Political Economy and Economic Development
# Author: Raimundo Eyzaguirre (reyzaguirre1@uc.cl)
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
library(lmtest)
library(sandwich)
library(multiwayvcov)

options(scipen=99)
getwd()
data <- import("jperoaddata.rdata")


#*****************************************************************************/
# 2.a  ----------------------------------------------------------------
#*****************************************************************************/

model <- lm(lndiffeall4mainancil ~ audit + und + fpm, data = data)

# Robust t test
coeftest(model, vcov = vcovHC(model, type = "HC1"))


#*****************************************************************************/
# 2.b  ----------------------------------------------------------------
#*****************************************************************************/

coeftest(model, cluster.vcov(model, data$kecid))


#*****************************************************************************/
# 2.c  ----------------------------------------------------------------
#*****************************************************************************/

model2 <- lm(lndiffeall4mainancil ~ audit + und + fpm + factor(auditstratnum), data = data)

coeftest(model2, cluster.vcov(model2, data$kecid))


#*****************************************************************************/
# 2.d  ----------------------------------------------------------------
#*****************************************************************************/

model3 <- lm(lndiffpall4 ~ audit + und + fpm, data = data)

# Robust t test
coeftest(model3, vcov = vcovHC(model3, type = "HC1"))


#*****************************************************************************/
# 2.e  ----------------------------------------------------------------
#*****************************************************************************/

coeftest(model3, cluster.vcov(model3, data$kecid))


#*****************************************************************************/
# 2.e  ----------------------------------------------------------------
#*****************************************************************************/

model4 <- lm(lndiffpall4 ~ audit + und + fpm + factor(auditstratnum), data = data)

coeftest(model4, cluster.vcov(model4, data$kecid))

