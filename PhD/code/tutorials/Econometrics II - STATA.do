*------------------------------------------------------------------------------*
/*-----------------------------------------------------------------------------*
Shuhei Kainuma
shuhei.kainuma@su.se
2024-04-04

Econometrics 2
Stata Tutorial do-file 

NB: This is for illustrative purpose. 
    Not all the commands on the slides are in this do-file.
*-----------------------------------------------------------------------------*/


* housekeeping
clear all // remove anything old stored
set more off, permanently   // Tell Stata not to pause.
set linesize 255            // Setting line size for the log file.
version                     // Check the version of the command interpreter.

cap log close // Close a log-file (if there's one open).
log using "stata_tutorial.log", replace



* Generating random numbers
clear
set obs 100 // no. of observations
set seed 100 // random seed
gen uni = runiform() // uniform distribution over [0 ,1]
gen norm = rnormal() // standard normal distribution
gen b = norm > 0.9 // 1 if random draw larger than 0.9, otherwise 0
// gen b = rnormal() > 0.9
gen b2 = inrange(uni, 0.4, 0.6) // 1 if uni takes values between 0.4 and 0.6, otherwise 0

gen b1 = 1 if norm > 0.9
replace b1 = 0 if norm <= 0.9 // in this case, you need two lines


* Missing values
clear 
set obs 100
gen m = . // numeric missing
count if m > 100 // returns 100 because . > 100 is satisfied

gen b = runiform() > 0.5
gen b2 = 1 if runiform() > 0.5 // b and b2 are the same or different?
sum b b2
sum b2 if !mi(b2)
// sum b2 if b2 != . // same

** tabulate variables
tab b
tab b2 // only shows the observations with nonmissing values
tab b2, mis // mis option shows the observations with missing values


* egen
** generated simulated data
set obs 100
gen age = floor(runiform(21, 40))
gen gender = runiform() > 0.5
gen earnings = exp(rnormal())
gen age_1 = floor(runiform(5, 15))
gen age_2 = floor(runiform(0, 10))
gen country = floor(runiform(0, 10))

bys age: egen max_earnings = max(earnings)
bys age gender: egen med_earnings = median(earnings)
egen ca = rowmax(age_1 age_2) if !mi(age_1) & !mi(age_2)
egen id_group = group(age gender country) // unique ID for group defined by the variables


* collapse (the followings cannot run at the same time)
collapse (mean) earnings, by(age) // mean earnings by age
collapse (mean) mean_earnings = earnings, by(age) // same as above, but a different variable
collapse (count) earnings, by(age) // count the number of obs. with nomissing values


* macro
global indepvar_model1 = "age education race"
local indepvar_model1 = "age education race"
dis "Print global macro like: $indepvar_model1"
dis "Print local macro like `indepvar_model1'"

** combining with for loop (see below)
foreach var of local indepvar_model1 {
  dis "`var'" // print each words
}


* regression
clear 
set obs 100
set seed 100
gen x1 = rnormal()
gen x2 = runiform()
gen e1 = rnormal()
gen y = 2*x1 + e1
reg y x1 // run regression


** absorbed or not absorbed?
gen x = floor(runiform(0, 10))
gen z = rnormal(0, 10)

** not absorbed
reg y i.x

** absorbed
areg y, absorb(x)
// ssc install reghdfe
reghdfe y, absorb(x)
reghdfe y, absorb(x, savefe)

** interacitons
reg y c.x#c.z 
reg y c.x##c.z
reg y i.x##c.z


** produce regression table
label variable x1 "Indep. Var. 1"
label variable x2 "Indep. Var. 2"
label variable y "Outcome"

eststo clear
eststo m1: reg y x1
eststo m2: reg y x2
esttab m*, se keep(x*) star(* 0.10 ** 0.05 *** 0.01) stat(N r2, label("N" "\$R^2\$")) label


* predict and residuals
reg y x1
predict predvar // predicted value
predict residvar, resid // residuals

** regression output
return list
ereturn list

gen r2 = e(r2)
local r2_tmp = e(r2)

mat list e(b)
gen beta1 = e(b)[1,1]


* save regression coefficients
regsave // see the data viwer


* Stata program
clear
cap program drop f1
program define f1
	cap drop _all
	set obs 100
	gen xx = runiform()
	gen yy = 3*xx +  rnormal()
	reg yy xx
end

simulate _b, reps(1000) seed(1234): f1 // _b is regression coefficient in Stata
hist _b_xx
scatter _b_cons _b_x


** can also work with loop (can be done more easily)
cap mkdir tmp // create the directory `tmp' if not exist
cap erase tmp/regsave_tmp.dta // remove `regsave_tmp.dta' file if exists

forvalue i = 1/1000 {

	cap drop _all
	* simulate data
	set obs 100
	gen xx = runiform()
	gen yy = 3 * xx + 1 + rnormal()

	* regression
	qui reg yy xx // qui suppresses output
	
	* save regression coefs as dataset
	regsave
	gen sim = `i'
	gen varnum = var == "xx"
	drop var stderr N r2
	qui reshape wide coef, i(sim) j(var)
	// qui save tmp/regsave_tmp`i'.dta, replace

	* append the estimates from each simulation into one dataset
	cap append using tmp/regsave_tmp.dta
	save tmp/regsave_tmp.dta, replace
	dis "The `i'-th simulation done."
}

hist coef1 // coef on xx
scatter coef0 coef1 


* graphs and schemes
** scatter plot + fitted line
sysuse auto.dta, clear
graph twoway (lfit price mpg) (scatter price mpg), ///
	graphregion(color(white))
graph twoway (lfit price mpg) (scatter price mpg), scheme(lean2) // with a different scheme

** time-series line plot + range plot of high & low prices
** (you can use rcap for plotting confidence intervals)
sysuse sp500, clear
graph twoway ///
	(line close date, lcolor(gray)) ///
	(rcap low high date) ///
	if _n < 80, ///
	graphregion(color(white))


* expand
clear
set obs 10 // no. of observations
set seed 100 // random seed
gen uni = runiform() // uniform distribution over [0 ,1]
gen norm = rnormal() // standard normal distribution
gen b = norm > 0.9 // 1 if random draw larger than 0.9, otherwise 0
// gen b = rnormal() > 0.9
gen b2 = inrange(uni, 0.4, 0.6) // 1 if uni takes values between 0.4 and 0.6, otherwise 0


expand 2 //, g(d)


log close