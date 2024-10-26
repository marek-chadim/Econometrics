*------------------------------------------------------------------------------*
/*-----------------------------------------------------------------------------*
Shuhei Kainuma
shuhei.kainuma@su.se
2024-04-23

Econometrics 2
Problem Set 2: Question 3

NB: start Stata by opening this do file so that the working directory automatically
	becomes the directory in which the do file is stored.
*-----------------------------------------------------------------------------*/

* housekeeping
clear all // remove anything old stored
set more off, permanently   // Tell Stata not to pause.
set linesize 255            // Setting line size for the log file.
version                     // Check the version of the command interpreter.

cap mkdir output // create a directory to store output (e.g. figures/tables)
cap mkdir log // create a directory to store log file if not exists

cap log close // Close a log-file (if there's one open).
log using log/PS2_Q3.log, replace


set scheme lean2

scalar rseed = 2304


* 3.a ==========================================

** define the program for the simulation
cap program drop experiment
program define experiment, rclass
	syntax [, probd(real 0.5)]
	
  cap drop random d x* y*

	gen random = runiform()
	sort random
	gen d = (_n/_N) <= `probd'
	
	gen x0 	= eps > -1
	gen x1 	= eps > 1
	gen x  	= d * x1 + (1 - d) * x0 
	gen y0 	= 5 * x0 + eps 
	gen y1 	= tau + 5 * x1 + eps
	gen y  	= d * y1 + (1 - d) * y0
	
  * Q3.b
	qui reg y d
	return scalar _b_d=_b[d]
	
  * Q3.c
  ** x == 1
	qui reg y d if x == 1
	return scalar _b_d_x1=_b[d]
	** x == 0
	qui reg y d if x == 0
	return scalar _b_d_x0=_b[d]
end


** generate sample (tau and epsilon)
set seed `=rseed'
set obs 1000

gen tau = rnormal(1, 1)
gen eps = rnormal(0, 1)
	

** run the simulation (generate the relevant variables)
experiment

sum tau
scalar tau_mean = `r(mean)'
cap gen te = y1 - y0 
sum te
scalar SATE = `r(mean)'

dis "SATE = `=SATE', Average tau = `=tau_mean'"

dis "ATE =" 1 + 5 * (normal(-1) - normal(1))




* 3.b & c ==========================================


simulate _b_d = r(_b_d) _b_d_x1 = r(_b_d_x1) _b_d_x0 = r(_b_d_x0), ///
	reps(1000) seed(`=rseed'): experiment, probd(0.5)
	
lab variable _b_d "(b) Full sample"
lab variable _b_d_x1 "(c) Subgroup $ \{i:X_i=1\} $"
lab variable _b_d_x0 "(c) Subgroup $ \{i:X_i=0\} $"

eststo clear
eststo: estpost tabstat _b_d*, s(mean) columns(statistics)
esttab using output/Tab_3.tex, ///
  cells(mean(fmt("%9.3fc"))) label unstack ///
  noobs nonumbers collabels("Average estimate of coefficient on $ D_i $") ///
  nomtitles nonotes substitute(\_ _) ///
  replace


* 3.d ==========================================
dis "Average diff. in epsilon for X = 1: " (normalden(1) / (1 - normal(1))) - (normalden(-1) / (1 - normal(-1)))
dis "average diff. in epsilon for X = 0: " -(normalden(1) / normal(1)) + (normalden(-1) / normal(-1))



log close
