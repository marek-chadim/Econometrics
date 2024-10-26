
clear all
set more off

/*------------------------------------------------------------------------------*

						Assignment A4
						Empirical exercise 
					
input: 1. Data_A4

*-------------------------------------------------------------------------------*/

*Set path 
global data ""
cd "$data"

*create a log file
*log using A4.log, replace

*load the data
use Data_A4, clear
gen t=_n

*tell stata you are working with time series
generate quarter = q(1960q1) + _n-1
format quarter %tq
tsset quarter

*plot the data
line  tbill r5 r10 quarter
graph export graph.eps, replace


* Ex. 4a
* Unit root pretesting
foreach var of varlist tbill r5 r10 {
	
  forvalues i=1/10 {
  dfuller `var', lags( `i') regress   
}
}

* Ex. 4b 
regress tbill r5 r10
outreg2 using table1.tex , replace stats(coef tstat)

*Get the residuals to perform the Engle Granger test
predict res1, res

* Plot the residuals
line res1 t, yline(0)
graph export res1.eps, replace

forvalues i=1/10 {
  dfuller res1, lags(`i') noconstant regress
}


*Ex. 4c
regress r10 tbill r5 
outreg2 using table2.tex , replace stats(coef tstat)

*Get the residuals to perform the Engle Granger test
predict res2, res

* Plot the residuals
line res2 t, yline(0)
graph export res2.eps, replace

 forvalues i=1/10 {
  dfuller res2, lags( `i') nocons regress   
}


* Ex. 4d
*rank test
vecrank tbill r5 r10, trend(rconstant) lags(8) max

*estimate
vec r5 tbill r10, trend(rconstant) lags(8) rank(1)


* Ex. 4e


reg r5 r10
predict res3, res
 forvalues i=1/10 {
  dfuller res3, lags( `i') nocons regress   
}

reg r5 tbill
predict res4, res
 forvalues i=1/10 {
  dfuller res4, lags( `i') nocons regress   
}

reg tbill r10 
predict res5, res
 forvalues i=1/10 {
  dfuller res5, lags( `i') nocons regress   
}

reg r10 r5
predict res6, res
 forvalues i=1/10 {
  dfuller res6, lags( `i') nocons regress   
}

reg tbill r5 
predict res7, res
 forvalues i=1/10 {
  dfuller res7, lags( `i') nocons regress   
}
reg r10 tbill  
predict res8, res
 forvalues i=1/10 {
  dfuller res8, lags( `i') nocons regress   
}
