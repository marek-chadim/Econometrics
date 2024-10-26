
clear all
set more off

/*------------------------------------------------------------------------------*

					Suggested solutions assignment A3
					Empirical exercise (ex. 10 p. 340)
					
input: 1. Data_A3

*-------------------------------------------------------------------------------*/

*Set path 
global data ""

cd "$data"

*load the data
use "Data_A3.dta", clear 

lab var indpro "industrial production"
lab var urate  "undemployment rate"
lab var r10    "interest rate 10y"
lab var tbill  "tbill"

*create a time varianle to use it later
gen t=_n
label var t "time"

*tell stata you are working with time series
generate quarter = q(1960q1) + _n-1
format quarter %tq
tsset quarter

*generate variables from q9 pg 339, which needed to procede in the exercise

	*difference of industrial production
	gen lip=log(indpro)
	label var lip "log indpro"
	
	gen dlip=D.lip
	labe var dlip "1st diff. log industrial production"

	*seasonal difference
	gen dur=D.urate
	lab var dur "1st diff. unemployment rate"
	
	*generate spread
	gen spread= r10-tbill
	lab var spread "spread"

*graph
twoway (line spread dur quarter, yaxis(1)) (line dlip quarter, yaxis(2)), title(Plot of the series) legend(label(2 "Diff. Unempl. Rate") /*
*/ label(1 "Spread") label(3 "Log change of ind. production")) 
graph export "series.png", replace


*Ex 10.a and b 

	*estimate the model
	var dlip dur spread, lags(1/3) 

	*testing granger causality in three different ways
	vargranger 

	test [dlip]L1.spread [dlip]L2.spread [dlip]L3.spread // Chi2-statistics 
	test [dur]L1.spread [dur]L2.spread [dur]L3.spread
	//test [dur]L1.spread=[dur]L2.spread=[dur]L3.spread=0 
	
	test [dlip]L1.spread [dlip]L2.spread [dlip]L3.spread, df(180) // F-statistics
	test [dur]L1.spread [dur]L2.spread [dur]L3.spread, df(180) 


*Ex 10.c

	*covar matrix
	matrix sig_var = e(Sigma)
	mat list sig_var

	*correlation matrix of the variance matrix
	matrix corr_var = corr(sig_var)
	matrix list corr_var

*Ex 10.d

	* estimate the model
	var dlip dur spread, lags(1/3) 
	
	*diagnostic check for serial correlation
	varlmar, mlag(12) 

	*IRFs
	irf set res
	irf create res, replace
	irf table fevd, noci
	
*Ex 10.e

	*estimate the model with levels (non-stationarity)
	varsoc lip urate spread, maxlag(5) 
	var lip urate spread, lags(1/5)  

	*diagnostic check for serial correlation
	varlmar, mlag(12) 

	*IRFs
	irf set res2
	irf create res2, replace
	irf table fevd, noci 

*Ex 10.f

	* plotting IRFs
	var dlip dur spread, lags(1/3) 
	irf set order1, replace

	*plotting IRF
	irf create order1, order(dlip dur spread) replace
	irf graph irf 
	graph export 10f.eps, replace

	*plotting OIRF
	irf graph oirf 
	graph export 10f2.eps, replace
	
	irf graph irf, irf(order1) impulse(dlip) response(dur) 
	graph export 10f3.eps, replace


*Ex 10.g (inverse order)

	var spread dur dlip, lags(1/3)
	irf set order2, replace

	*plotting IRF
	irf create order2, order(spread dur dlip) replace  
	irf graph irf 
	graph export 10g.eps, replace

	*plotting OIRF
	irf graph oirf 
	graph export 10g2.eps, replace

	irf graph irf, irf(order2) impulse(dlip) response(dur)
	graph export 10g3.eps, replace
	
*END
