*------------------------------------------------------------------------------*
/*-----------------------------------------------------------------------------*
Shuhei Kainuma
shuhei.kainuma@su.se
2024-04-23

Econometrics 2
Problem Set 2: Question 2

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
log using log/PS2_Q2.log, replace


set scheme lean2

scalar rseed = 2304


* 2.b ==========================================

set seed `=rseed'
set obs 1000

gen eps = rnormal(0, 1)
gen tau = rnormal(1, 1)
gen x  = (_n > _N/2)
gen y0 = 5 * x 	+ eps 
gen y1 = y0 + tau

sum tau
scalar SATE = `r(mean)'
dis "Sample Average Treatment Effect = `=SATE'"



* 2.c ==========================================

** define the programs for simulation
*** program for treatment assignment
cap program drop assign_treatment 
program define assign_treatment, sortpreserve
	syntax [, probd(real 0.25) stratify(varlist)]
	gen random = runiform()
	sort `stratify' random
	if ("`stratify'" != "") {
		by `stratify': gen d = (_n/_N) <= `probd'
	}
	else {
		gen d = (_n/_N) <= `probd'
	}
	drop random
end

*** program for simulation, with `assignment_random' for treatment assignment
cap program drop assignment_random
program define assignment_random, rclass
	syntax [, probd(real 0.25) stratify(varlist)]
	if ("`stratify'" != "") {
		assign_treatment, probd(`probd') stratify(`stratify')
	}
	else {
		assign_treatment, probd(`probd')
	}
	gen y = y1 * d + (1 - d) * y0 
	reg y d
	return scalar beta = _b[d]
	return scalar ster = _se[d]
	test _b[d] == `=SATE'
	return scalar pval = r(p)
	drop d y random
end


** run simulation with 25% obs = treated
preserve
	set seed `=rseed'
	simulate beta_c = r(beta) ster_c = r(ster) pval_c = r(pval), ///
		reps(1000) saving(output/q2_c.dta, replace): assignment_random
restore



* 2.d ==========================================

** run simulation with 50% obs = treated
preserve
	set seed `=rseed'
	simulate beta_d = r(beta) ster_d = r(ster) pval_d = r(pval), ///
		reps(1000) saving(output/q2_d.dta, replace): assignment_random, probd(0.5)
restore




* 2.e ==========================================

** run simulation with 50% obs = treated
preserve
	set seed `=rseed'
	simulate beta_e = r(beta) ster_e = r(ster) pval_e = r(pval), ///
		reps(1000) saving(output/q2_e.dta, replace): assignment_random, stratify(x)
restore


* 2.f ==========================================

** create the data with simulated regression estimates, SEs, and p-values 
use output/q2_c, clear
merge 1:1 _n using output/q2_d, nogen
merge 1:1 _n using output/q2_e, nogen


** plot distributions of the coefficient estimates from (c), (d), and (e)
twoway (kdensity beta_c) (kdensity beta_d) (kdensity beta_e), ///
		xline(1, lcolor("red") lpattern("dash")) ///
		legend(order(1 "(c) Random Assignment (25%)" 2 "(d) Random Assignment (50%)" 3 "(e) Stratified Random Assignment (25%)" ) position(6) cols(1)) ///
		xtitle("{&tau}")
graph export output/Fig_2f.png, replace
		


* 2.g ==========================================

gen id = _n
reshape long beta ster pval, i(id) j(Question) string
drop id

gen pval_below5 = pval < 0.05

label variable beta "Coefficient Estimates"
label variable ster "SE"
label variable pval_below5 "% \ p-value < 0.05"

foreach w in c d e {
	replace Question = "(`w')" if Question == "_`w'"
}


table () Question, statistic(sd beta) statistic(mean ster pval_below5) nformat(%9.3f) nototal
collect style tex, nobegintable
collect style cell border_block, border(right, pattern(nil))
collect export output/Tab_2g.tex, replace tableonly





log close
