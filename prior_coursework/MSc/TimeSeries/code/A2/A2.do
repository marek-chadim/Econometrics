

clear all
set more off

/*------------------------------------------------------------------------------*

					Suggested solutions assignment A2
							Empirical exercise
					
input: 
		1. A2_SEK_USD_ExchangeRates.dta
		2. A2_IP_US.dta
		3. A2_IP_UK

*-------------------------------------------------------------------------------*/

*Set path 
global data ""
global tables ""
cd "$data"

*Open log file
log using "A2.log", replace

*Load the dataset
use "A2_SEK_USD_ExchangeRates.dta", clear 

* Part (a) * 
use A2_SEK_USD_ExchangeRates.dta, clear

*rename to simplify
rename SEKUSD y

***** Graph the data *****
tsline y, title("SEK/US exchange rate") 
graph export "$tables/v12.png", replace  

***Lag selection using AIC and BIC - as in the PC Lab ***
*k=0
reg D.y
estat ic
*k=1
reg D.y LD.y
estat ic
*k=2
reg D.y LD.y L2D.y
estat ic
*k=3
reg D.y LD.y L2D.y L3D.y
estat ic
*k=4
reg D.y LD.y L2D.y L3D.y L4D.y
estat ic
*k=5
reg D.y LD.y L2D.y L3D.y L4D.y L5D.y
estat ic
*k=6
reg D.y LD.y L2D.y L3D.y L4D.y L5D.y L6D.y
estat ic

***white noise test k=5***
reg D.y LD.y L2D.y L3D.y L4D.y L5D.y
predict ehat, residuals
wntestq ehat, lags(1)
wntestq ehat, lags(4)
wntestq ehat, lags(8)
wntestq ehat, lags(12)
tsline ehat //behaviour like a WN. 

***white noise test k=2***
reg D.y LD.y L2D.y
predict ehat2, residuals
wntestq ehat2, lags(1)
wntestq ehat2, lags(4)
wntestq ehat2, lags(8)
wntestq ehat2, lags(12)
tsline ehat2

***ADF test***
dfuller y, trend regress lags(5)
dfuller y, regress lags(5)

**** ERS test **** 
*trend is default
dfgls y, ers maxlag(6)


/*** General questions asked during the seminars ***

1) ers option
-> When we compute the DF-GLS test in Stata, we have the possibility of using the ERS option. When we use this option, Stata present interpolated critical values from tables presented by Elliot, Rothenberg, and Stock (1996), which they obtained from simulation. Otherwise, by default Stata uses the critical values computed from the response surface analysis of Cheung and Lai (1995). The difference between these two critical values is related to the sample size of the series, but it is also outside for the purpose of this course, so do not worry about it. 

2) In general, what should we do if the ADF and the DG-GLS indicate different results?
-> You should always stick to the DG-GLS results as it has more power. 

3) When we estimated the model, we found to suitable models using the AIC and the BIC conditions, but when we did the ADF test, we got opposite results. In general, what do we do if this happens?
-> If ADF test rejects unit root by AIC, but not BIC, always precede with the rejection, as it is a sign of strong evidence; while the failing to reject is what is defined as weak evidence. 

4) Always about the DF-GLS command, why is the maxlag at 6? How to choose the max value?
-> Maxlag sets the value of the higher lag order for the first differenced, detrended variable in the DF regression. By default, dfgls sets k according to the method proposed by Schwert (1898). However, we can overwrite this option with the maxlag option. There are many rules of thumb to use, the one we follow in this course if this
p_max  = q*[T/100]^(2/9)   
Where q depends on the frequency of the data (year=1, quarterly=4, monthly = 12). 

5) When we estimated the model, we found to suitable models using the AIC and the BIC conditions, but when we did the ADF test, we got opposite results. In general, what do we do if this happens?
-> If ADF test rejects unit root by AIC, but not BIC, always precede with the rejection, as it is a sign of strong evidence; while the failing to reject is what is defined as weak evidence. 

*/

***Test for structural break***
cusum6 D.y LD.y L2D.y L3D.y L4D.y L5D.y if t>=10, rr(rescs) cs2(squarescs)  lw(lowband) uw(upperband) cs(csumtest)
line  upperband lowband csumtest t
graph export "$tables/csm.png", replace

*****************************************
*****************Part (b)****************
*****************************************

clear all
set more off

*** Merge the datasets - A2_IP_US.dta and A2_IP_UK.dta **
use "$data/A2_IP_US.dta", clear
cap rename IP_US US
save "$data/A2_IP_US.dta", replace

use "$data/A2_IP_UK.dta", clear
cap rename IP_UK UK
save "$data/A2_IP_UK.dta", replace

merge 1:1 _n using "$data/A2_IP_US.dta"
save "$data/A2_b.dta", replace 

*** Load the data, tell stata that you are working with time series ***
use "$data/A2_b.dta", clear 
tsset quarter

*** Plot the graphs for visual inspection ***
tsline US, title(US IP Series)
graph export "$tables/US.png", replace
tsline UK, title(UK IP Series)
graph export "$tables/UK.png", replace

ac US, ciopts(col(white))
graph export "$tables/USac.png", replace
ac UK, ciopts(col(white))
graph export "$tables/UKac.png", replace

*** ADF test - Lag selection - using AIC and BIC. ***
*US
*k=0
reg D.US
estat ic
*k=1
reg D.US LD.US
estat ic
*k=2
reg D.US LD.US L2D.US
estat ic
*k=3
reg D.US LD.US L2D.US L3D.US
estat ic
*k=4
reg D.US LD.US L2D.US L3D.US L4D.US
estat ic
*k=5
reg D.US LD.US L2D.US L3D.US L4D.US L5D.US
estat ic
*k=6
reg D.US LD.US L2D.US L3D.US L4D.US L5D.US L6D.US
estat ic 


*UK
*k=0
reg D.UK
estat ic
*k=1
reg D.UK LD.UK
estat ic
*k=2
reg D.UK LD.UK L2D.UK
estat ic
*k=3
reg D.UK LD.UK L2D.UK L3D.UK
estat ic
*k=4
reg D.UK LD.UK L2D.UK L3D.UK L4D.UK
estat ic
*k=5
reg D.UK LD.UK L2D.UK L3D.UK L4D.UK L5D.UK
estat ic
*k=6
reg D.UK LD.UK L2D.UK L3D.UK L4D.UK L5D.UK L6D.UK
estat ic

***white noise test***
reg D.US LD.US
predict ehat2, residuals
wntestq ehat2, lags(1)
wntestq ehat2, lags(4)
wntestq ehat2, lags(8)
wntestq ehat2, lags(12)
tsline ehat2

reg D.US LD.US L2D.US L3D.US L4D.US L5D.US
predict ehat1, residuals
wntestq ehat1, lags(1)
wntestq ehat1, lags(4)
wntestq ehat1, lags(8)
wntestq ehat1, lags(12)
tsline ehat1

reg D.UK 
predict ehat3, residuals
wntestq ehat3, lags(1)
wntestq ehat3, lags(4)
wntestq ehat3, lags(8)
wntestq ehat3, lags(12)
tsline ehat3

reg D.UK LD.UK L2D.UK L3D.UK L4D.UK L5D.UK L6D.UK
predict ehat4, residuals
wntestq ehat4, lags(1)
wntestq ehat4, lags(4)
wntestq ehat4, lags(8)
wntestq ehat4, lags(12)
tsline ehat4

***ADF test***
dfuller US, trend regress lags(5)
dfuller US, regress lags(5)

dfuller UK, trend regress lags(6)
dfuller UK, regress lags(6)

*** ERS test ***
dfgls US, maxlag(6) 
dfgls UK, maxlag(6)

*** Perron test ***
gen t = _n 

*UK*
tsline UK, title(UK IP Series)

*Create level dummy 
gen dl=0
replace dl=1 if t>81

*create pulse dummy (1 at t*+1 (1981)) 
gen dp=0
replace dp=1 if t==82

*create trend slope dummy 
gen dt=_n - 81
replace dt=0 if dt<0

reg UK L1.UK t dp dl dt
predict yhat, residual 
tsline yhat 
graph export "$tables/yhat.png", replace

*detrend 
reg UK t dp dl dt
predict EstTrend1, xb 
tsline UK EstTrend1 
graph export "$tables/pred.png", replace

* WN test
wntestq yhat, lags(4)
wntestq yhat, lags(8)
wntestq yhat, lags(12)

display (.97813829 -1)/  .0141811 // t-stat
// Note that T =228 and τ =81, so λ=0.355. The critical values are −4.81 at 1%, −4.22 at 5% and −3.95 at 10%.

* run equations with lags
reg UK L1.UK t dp dl dt
estat ic
reg UK L1.UK LD.UK t dp dl dt
estat ic
reg UK L1.UK LD.UK L2D.UK t dp dl dt
estat ic
reg UK L1.UK LD.UK L2D.UK L3D.UK t dp dl dt
estat ic
reg UK L1.UK LD.UK L2D.UK L3D.UK L4D.UK t dp dl dt
estat ic
reg UK L1.UK LD.UK L2D.UK L3D.UK L4D.UK L5D.UK t dp dl dt
estat ic
reg UK L1.UK LD.UK L2D.UK L3D.UK L4D.UK L5D.UK L6D.UK t dp dl dt
estat ic

**check 
reg UK L1.UK L2.UK L3.UK L4.UK L5.UK L6.UK t dp dl dt
predict yhat1_AIC, residual
wntestq yhat1_AIC, lags(4)
wntestq yhat1_AIC, lags(8)
wntestq yhat1_AIC, lags(12) 

*US* 
drop dl dt dp yhat EstTrend1 
tsline US, title(US IP Series)

*Create DL level shift dummy 
gen dl=0
replace dl=1 if t>196

*create pulse dummy
gen dp=0
replace dp=1 if t==197

*create trend slope dummy
gen dt=_n - 196
replace dt=0 if dt<0

*model in step 1 
reg US L1.US t dl dt dp
predict yhat, residual
tsline yhat
graph export "$tables/yhat1.png", replace

*detrend
reg US t dl dt dp
predict EstTrend1, xb
predict xhat, residuals
tsline US EstTrend1
graph export "$tables/pred1.png", replace

** WN test
wntestq yhat, lags(4)
wntestq yhat, lags(8)
wntestq yhat, lags(12) 

*AIC and BIC method
*k=0
reg US L1.US t dp dl dt
estat ic
*k=1
reg US L1.US LD.US t dp dl dt
estat ic
*k=2
reg US L1.US LD.US L2D.US t dp dl dt
estat ic
*k=3
reg US L1.US LD.US L2D.US L3D.US t dp dl dt
estat ic
*k=4
reg US L1.US LD.US L2D.US L3D.US L4D.US t dp dl dt
estat ic
*k=5
reg US L1.US LD.US L2D.US L3D.US L4D.US L5D.US t dp dl dt
estat ic
*k=6
reg US L1.US LD.US L2D.US L3D.US L4D.US L5D.US L6D.US t dp dl dt
estat ic  

*WN test
reg US L1.US  t dp dl dt
predict yhat2, residual
wntestq yhat2, lags(4)
wntestq yhat2, lags(8)
wntestq yhat2, lags(12)

reg US L1.US LD.US L2D.US L3D.US L4D.US L5D.US L6D.US t dp dl dt
predict yhat3, residual
wntestq yhat3, lags(4)
wntestq yhat3, lags(8)
wntestq yhat3, lags(12)

// The critical values for the Perron test (λ = 0.9) are −4.41 at 1%, −3.80 at 5% and −3.46 at 10%.
display  (.9749296  -1)/ .0114406  

/*** General questions asked during the seminars on exercise 2.b ***
**
1) In exercise 2.b, if the UK AIC value is always decreasing, how do we choose the maximum number of lags to use?
You can use the above p_max formula. 

2) When we estimate the Perron test in the PC labs we did differently compared to the book. What does it change between the two versions? I get different values when I compute it. Why? Which one should I proceed with?
-> In the PD LAB you followed a two steps procedure, which is usually easier to understand. However, this procedure can be done in only one step as it is explained in the book. Note that outcomes between the two methods cannot be very different. 
*/


*END
log close
