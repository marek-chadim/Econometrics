*------------------------------------------------------------------------------*
/*-----------------------------------------------------------------------------*
Shuhei Kainuma
shuhei.kainuma@su.se
2025-04-16

Econometrics 2
Problem Set 1: Question 2

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
log using log/PS1_Q2.log, replace


set scheme lean2



* 2.f ==========================================

clear
set seed 24
set obs 200

gen y0 = rnormal()

gen d = runiform() < .5

local p = .3
gen x = runiform() < `p'

tab d x // check that non-zero in all four cells

local alpha0 = 1
local alpha1 = -2
gen tau_i = `alpha0' + `alpha1'*x

gen y = y0 + d*tau_i

preserve

	collapse y, by(d)
	gen tau_hat = y - y[_n-1]
	list tau_hat in 2

restore

reg y d




* 2.g ==========================================

preserve

	collapse y, by(d x)
	sort x d
	gen tau_x_hat = y - y[_n-1]
	list tau_x_hat if x == 0 & d == 1 // tau(0)
	list tau_x_hat if x == 1 & d == 1 // tau(1)

restore

gen dx = d*x
qui reg y d x dx
lincom d // tau(0)
lincom dx + d // tau(1)

gen dnx = d * (1-x)
reg y dx dnx x



* 2.h ==========================================

cap program drop expersim
program define expersim, rclass
	syntax [, obs(integer 1) exper(integer 1) p(real .3) pd_exper1(real 0.5)]
	clear
	set obs `obs'
	
	gen y0 = rnormal()
	
	local p = `p' // share women
	local q = `p'/(1-`p') // number of women per man
	gen u = (1+`q')*runiform() // [0,1): men; [1,1+q]: women
	
	// gen x = u >= 1
	sort u
	gen x = _n >= (1 - `p') * `obs' + 1
	if `exper' == 1 {
		gen d = runiform() <= `pd_exper1' //(1+`q')/2
	}
	else if `exper' == 2 {
		// gen d = u < 2/3 | u >= 1+`q'/2 // bottom 2/3 of men, top half of women
		gen order_r = runiform()
		sort x order_r
		gen d = _n <= (`p' * `obs') | _n >= (1 - `p') * `obs' + (`p' * `obs')/2 + 1
	}
	else di "choose either exper(1) or exper(2)"

	tab d x
	
	local alpha0 = 1
	local alpha1 = -2
	gen tau_i = `alpha0' + `alpha1'*x
	
	gen y = y0 + d*tau_i
	
	reg y d
	
	return scalar tau = _b[d]
	
	gen dx = d*x
	reg y d x dx
	
	return scalar tau0 = _b[d]
	lincom dx + d
	return scalar tau1 = `r(estimate)'
end

forval k = 1/2 {
	simulate tau_exp`k'=r(tau) tau0_exp`k'=r(tau0) tau1_exp`k'=r(tau1), ///
		reps(500) seed(12344): expersim, ///
		obs(100) exper(`k') p(0.3) 
		
	gen id = _n
	tempfile est`k'
	save `est`k''
}

use `est1', clear
merge 1:1 id using `est2'
tw (kdensity tau_exp1, xline(.4)) (kdensity tau_exp2, xline(.4)), ///
	legend(order(1 "Experiment 1" 2 "Experiment 2") rows(1) position(6))
graph export output/Fig_2h_att.png, replace

tw (kdensity tau0_exp1, xline(1)) (kdensity tau0_exp2, xline(-1)) ///
	(kdensity tau1_exp1, xline(1)) (kdensity tau1_exp2, xline(-1)), ///
	legend(order(1 "x = 0 (Exper.1)" 2 "x = 0 (Exper.2)" 3 "x = 1 (Exper.1)" 4 "x = 1 (Exper.2)") rows(2) position(6))
graph export output/Fig_2h_catt.png, replace




log close