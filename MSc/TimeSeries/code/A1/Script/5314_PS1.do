********************************************************************************
************** 5314 Applied Econometric Time Series Spring 2024 PS1 ************
*********************** Solutions by Team Fast & Spurious **********************
********************************************************************************

********************************************************************************
*********************** PRELIMINARIES AND LOADING DATA *************************
********************************************************************************
	clear all
	set more off
	global root "C:\Users\chadi\Dropbox\5314\5314_PS1"
	global data "$root\Data"
	global output "$root\Output"
	log using log.txt
* Loading data
	import delimited "$data\US_spread.txt", clear

********************************************************************************
********************************** ANALYSIS ************************************
********************************************************************************


// UNIVARIATE TIME SERIES ANALYSIS
//------------------------------------------------------------------------------
	
// String to date variable
	codebook, compact
	tostring year, generate(str_year)
	tostring quarter, generate(str_quarter)
	generate date = str_year + "/" + str_quarter
	generate t = quarterly(date,"YQ")
	format t %tq
	drop str_year str_quarter date 
	
// Tell Stata our data are time series and restrict period for replication
	tsset t
	keep if tin(1960q1, 2012q4)
	gen dspread = D.spread
	codebook, compact
	
// Plots
	tsline spread, ytitle("") saving(${output}/series) 
	tsline dspread, ytitle("") saving(${output}/dseries)
	graph combine ${output}/series.gph ${output}/dseries.gph, col(1)
	graph export "${output}/tsline.png", width(4000) replace


	ac spread, lags(12) saving(${output}/ac) //computing the ACF for the spread series using up to 12 lags (s = 12)
	pac spread, lags(12) saving(${output}/pac) //computing the PACF for the spread series using up to 12 lags (s = 12)
	graph combine ${output}/ac.gph ${output}/pac.gph, col(1)
	graph export "${output}/cor.png", width(4000) replace

* Model 1: ARIMA with p=1 to 7, q=0
	arima spread if tin(1961q4, ), ar(1/7)   
	predict residuals1, resid
	corrgram residuals1, lag(12)
	est store model1
	estat ic

* Model 2: ARIMA with p=1 to 6, q=0
	arima spread if tin(1961q4, ), ar(1/6)
	predict residuals2, resid
	corrgram residuals2, lag(12)
	est store model2
	estat ic

* Model 3: ARIMA with p=1 and 2, q=0
	arima spread if tin(1961q4, ), ar(1/2)
	predict residuals3, resid
	corrgram residuals3, lag(12)
	est store model3
	estat ic

* Model 4: ARIMA with p=1,2 and 7, q=0
	arima spread if tin(1961q4, ), ar(1/2,7)
	predict residuals4, resid
	corrgram residuals4, lag(12)
	est store model4
	estat ic

* Model 5: ARIMA with p=1, q=1
	arima spread if tin(1961q4, ), ar(1) ma(1)
	predict residuals5, resid
	corrgram residuals5, lag(12)
	est store model5
	estat ic

* Model 6: ARIMA with p=1 and 2, q=1
	arima spread if tin(1961q4, ), ar(1/2) ma(1)
	predict residuals6, resid
	corrgram residuals6, lag(12)
	est store model6
	estat ic

* Model 7: ARIMA with p=1 and 2, q=1 and 7
	arima spread if tin(1961q4, ), ar(1/2) ma(1,7)
	predict residuals7, resid
	corrgram residuals7, lag(12)
	est store model7
	estat ic
	
* Table
	esttab model1 model2 model3 model4 model5 model6 model7 using "${output}/table.tex",  stat(aic bic) replace


// Lag-order selection with arimamsoc
	arimasoc spread  if tin(1961q4, )


// Forecast

   arima spread if tin(, 2000q2), ar(1/7) //estimating an AR(7) process with a constant using observations from 1961q4 to 2000q2
   predict ISFspreadAR, dynamic(tq(2000q3)) //forecasting the spread series from 2000q3 to 2012q4 using an AR(1) process
   generate etAR=spread-ISFspreadAR //calculating forecast errors
   generate sqetAR=(etA^2) //the squared forecast errors
   generate MSPEAR=sum(sqetAR)/24 if quarter>tq(2000q2) //calculating accumulated MSPEs for the relevant period; in the data editor window and the last value in the corresponding column gives the desirable MSPE
   
 
   arima spread if tin(, 2000q2), ar(1/2) ma(1,7) //estimating an ARMA[2,(1,7)] process with a constant using observations from 1987q1 to 2005q4
   predict ISFspreadMA, dynamic(tq(2000q3)) //forecasting the spread series from 2000q3 to 2012q4 using an MA(1) process
   generate etMA=spread-ISFspreadMA //calculating forecast errors
   generate sqetMA=(etMA^2) //the squared forecast errors
   generate MSPEMA=sum(sqetMA)/24 if quarter>tq(2000q2) //calculating accumulated MSPEs for the relevant period; in the data editor window and the last value in the corresponding column gives the desirable MSPE

	tsline spread ISFspreadAR ISFspreadMA //plotting the actual series and the forecasts
	graph export "${output}/forecast.png", width(4000) replace
	
	
// Structural break

*1981q4

* Full model regression
	reg spread l1.spread l2.spread l3.spread l4.spread l5.spread l6.spread l7.spread

* Store information for full model
	scalar rss_full = e(rss)
	scalar df_full = e(N)
	dis df_full
	dis rss_full

* Pre-breakpoint model
	reg spread l1.spread l2.spread l3.spread l4.spread l5.spread l6.spread l7.spread if tin(, 1981q4)
* Store RSS of the pre-breakpoint model
 	scalar rss_pre = e(rss)
	dis rss_pre

* Post-breakpoint model
	reg spread l1.spread l2.spread l3.spread l4.spread l5.spread l6.spread l7.spread if tin(1982q1, ) 
* Store RSS of the post-breakpoint model
	scalar rss_post = e(rss)
	dis rss_post

* Calculate the Chow Test statistic
	scalar num = (rss_full - (rss_pre + rss_post)) / (8)
	scalar denom = (rss_pre + rss_post) / (df_full - 16)
	scalar chow = num / denom

* Display the Chow Test statistic
	di "Chow Test Statistic = " chow
* Calculate and display p-value
	scalar p_value = Ftail(8, df_full - 16, chow)
	di "P-value = " p_value
* Interpretation
	if p_value < 0.05 {
		di "Reject the null hypothesis: Evidence of a structural break at the breakpoint."
	} 
	else {
    di "Fail to reject the null hypothesis: No evidence of a structural break at the breakpoint."
}		

*1991q4

* Full model regression
	reg spread l1.spread l2.spread l3.spread l4.spread l5.spread l6.spread l7.spread

* Store information for full model
	scalar rss_full = e(rss)
	scalar df_full = e(N)
	dis rss_full

* Pre-breakpoint model
	reg spread l1.spread l2.spread l3.spread l4.spread l5.spread l6.spread l7.spread if tin(, 1991q4)	
* Store RSS of the pre-breakpoint model
	scalar rss_pre = e(rss)
	dis rss_pre

* Post-breakpoint model
	reg spread l1.spread l2.spread l3.spread l4.spread l5.spread l6.spread l7.spread if tin(1992q1, ) 
* Store RSS of the post-breakpoint model
	scalar rss_post = e(rss)
	dis rss_post

* Calculate the Chow Test statistic
	scalar num = (rss_full - (rss_pre + rss_post)) / (8)
	scalar denom = (rss_pre + rss_post) / (df_full - 16)
	scalar chow = num / denom

* Display the Chow Test statistic
	di "Chow Test Statistic = " chow

* Calculate and display p-value
	scalar p_value = Ftail(8, df_full - 16, chow)
	di "P-value = " p_value

* Interpretation
	if p_value < 0.05 {
		di "Reject the null hypothesis: Evidence of a structural break at the breakpoint."
	} 
	else {
		di "Fail to reject the null hypothesis: No evidence of a structural break at the breakpoint."
	}		


// Stability
	reg spread L.spread  
	estat sbcusum
	graph export "${output}/cusum.png", width(4000) replace
	
	gen Lspread = L.spread
	rolling _b _se, window(10) recursive saving(${output}/betas, replace) keep(t): reg spread Lspread
	use ${output}/betas, clear
	list , table
	generate lower = _b_Lspread - 1.96*_se_Lspread
	generate upper = _b_Lspread + 1.96*_se_Lspread
	generate lowerc = _b_cons - 1.96*_se_cons
	generate upperc = _b_cons + 1.96*_b_cons

	twoway (line _b_Lspread date) (rline lower upper date), ytitle("Beta") xtitle("Date") saving(${output}/ar)
	twoway (line _b_cons date) (rline lowerc upperc date), ytitle("Beta") xtitle("Date") saving(${output}/con)
	graph combine ${output}/ar.gph ${output}/con.gph, col(1) 
	graph export "${output}/stability.png", width(4000) replace


	capture log close
