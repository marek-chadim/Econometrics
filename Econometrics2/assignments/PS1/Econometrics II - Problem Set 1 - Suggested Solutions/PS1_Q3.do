*------------------------------------------------------------------------------*
/*-----------------------------------------------------------------------------*
Shuhei Kainuma
shuhei.kainuma@su.se
2024-04-16

Econometrics 2
Problem Set 1: Question 3

NB: start Stata by opening this do file so that the working directory automatically
		becomes the directory in which the do file is stored.
*-----------------------------------------------------------------------------*/

* housekeeping
clear all // remove anything old stored
set more off, permanently   // Tell Stata not to pause.
set linesize 255            // Setting line size for the log file.
version                     // Check the version of the command interpreter.

cap mkdir output // create a directory to store output (e.g. figures/tables) if not exists
cap mkdir log // create a directory to store log file if not exists

cap log close // Close a log-file (if there's one open).
log using log/PS1_Q3.log, replace


set scheme lean2




* 3.d ==========================================

set seed 24

cap program drop experiment
program define experiment, rclass 
	version 17.0
	syntax [, obs(integer 100) n1(integer 50) sigma(real 1) rho(real 0)]

	cap drop _all
	set obs `obs'
	matrix C = (`sigma', `rho' \ `rho', 1)
	drawnorm y1 y0, means(2, 1) cov(C) // draw y0 & y1 from the multivariate normal dist.

	gen random = runiform()
	sort random
	gen d = 0
	replace d = 1 in 1/`n1' // assign d = 1 to n_1 observations
	gen tau = y1 - y0  // individual treatment effect

	quiet sum tau 
	return scalar mean = r(mean) // true SATE

	gen y = y1 * d + y0 * (1 - d) // observed y
	quiet reg y d // diff-in-means
	return scalar d = _b[d] // estimate of the SATE
end


experiment
kdensity tau, xtitle("{&tau}")
graph export output/Fig_3d.png, replace



* 3.e ==========================================

** save obserevd outcome, treatment, and the regression result
experiment 

keep y d

reg y d 
scalar tau_hat = _b[d]  // SATE estimate 
scalar tau_hat_pval = r(table)[4, 1]  // p-value

save output/q3_assignment, replace 


** randomisation inference
cap program drop tau_hat_H0
program define tau_hat_H0
	version 17.0
	syntax [, n1(integer 50)]

	cap drop d
	cap drop random 
	gen random = runiform()
	sort random 
	gen d = 0
	replace d = 1 in 1/`n1' // generate alternative assignment

	quiet reg y d // y = y0 = y1 under H0
end

simulate _b, reps(1000): tau_hat_H0


** visualise the result
hist _b_d, ///
	fcolor(none) xtitle("{&tau}-hat") xlabel(0 0.5 1 1.5 2) ///
	width(0.05) xline(`=tau_hat', lcolor(red))
graph export output/Fig_3e.png, replace

** compute share of effects > estimate from the obserevd data
count if _b_d > `=tau_hat'
di `r(N)'/_N // fraction of coefficient estimates larger than that under the true treatment assignment 




* 3.f (bootstrap) ==========================================

cap program drop tau_hat_bsample 
program define tau_hat_bsample 
	use output/q3_assignment, clear
	bsample
	quiet reg y d
end

simulate _b, reps(1000): tau_hat_bsample 


** visualise the result
hist _b_d, ///
	fcolor(none) xtitle("{&tau}-hat") xlabel(0 0.5 1 1.5 2) ///
	width(0.05) xline(`=tau_hat', lcolor(red) lwidth(0.8))
graph export output/Fig_3f.png, replace

** compute share of estimates < 0
count if _b_d < 0
di `r(N)'/_N // fraction of coefficient estimates less than 0




log close