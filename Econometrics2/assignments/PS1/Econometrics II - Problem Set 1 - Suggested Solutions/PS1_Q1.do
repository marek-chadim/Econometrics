*------------------------------------------------------------------------------*
/*-----------------------------------------------------------------------------*
Shuhei Kainuma
shuhei.kainuma@iies.su.se
2024-04-16

Econometrics 2
Problem Set 1: Question 1

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
log using log/PS1_Q1.log, replace


set scheme lean2


* 1.5 ==========================================
set seed 42

local N = 100
set obs `N'

gen i = _n

** generate X
gen x = 1 if i <= `N'/2
  replace x = 2 if i > `N'/2 & i <= 3*`N'/4
  replace x = 3 if i > 3*`N'/4

** generate D
gen u = runiform()
gen d = u <= 1/3 if x == 1
  replace d = u <= 2/3 if x == 2
  replace d = 1 if x == 3
  label variable d "D"

tab x d, row // show share treated by X

gen y0 = runiform()
gen y1 = x*runiform()


* 1.6 ==========================================
** generate individual causal effects
gen tau_i = y1 - y0

** make kernel density plots by X_i
twoway (kdensity tau_i if x == 1) ///
	(kdensity tau_i if x == 2) ///
	(kdensity tau_i if x == 3), ///
  xtitle("{&tau}{subscript:i}") ///
  ytitle("Density") ///
  legend(order(1 "X=1" 2 "X=2" 3 "X=3") rows(1) position(6)) 
graph export output/Fig_1_6_kdensity.png, replace


* 1.7 ==========================================
** generate the observed outcome
gen y = y0*(1-d) + y1*d
  label variable y "Y"
	
eststo clear
eststo ate_att: reg y d // ATT (and ATE)

qui sum tau_i // ATE
  estadd scalar ate_true = r(mean) : ate_att

qui sum tau_i if d == 1 // ATT
  estadd scalar att_true = r(mean) : ate_att

** estimate CATT
*** subsetting data
forval x = 1/3 {
  * estimation
	eststo catt_x`x': reg y d if x == `x'

  * obtain true value
  qui sum tau_i if d == 1 & x == `x'
    estadd scalar catt_true = r(mean) : catt_x`x'
}

** stacked specification
forval x = 1/3 {
	gen d_`x' = 0
	  replace d_`x' = d if x == `x'
    label variable d_`x' "D $\times 1\{X=`x'\}$"
}
eststo catt_stack: reg y i.x d_*

** estimate ATE
*** ATU 
scalar drop _all
forval x = 1/2 {
  * Pr(D | X)
  cap gen x`x' = x == `x'
  qui sum x`x' if d == 0
    scalar pr_x`x' = r(mean)

  * CATT
  qui reg y d if x == `x' 
    scalar catt_x`x' = _b[d] 
}

scalar atu_est = `=pr_x1' * `=catt_x1' + `=pr_x2' * `=catt_x2'
// dis `=atu_est'

*** ATE as a weighted average of ATT and ATU
qui sum d 
  scalar pr_d = r(mean)

qui reg y d 
  scalar att_est = _b[d] // att
  
estadd scalar ate_est = `=pr_d' * `=att_est' + (1 - `=pr_d') * `=atu_est' : ate_att // ATE estimate



esttab ate_att catt_x* catt_stack using output/Tab_1_7.tex, ///
  nostar se noobs label replace ///
  keep(d*) ///
  mtitles("ATE/ATT" "X=1" "X=2" "X=3" "CATT: Stacked Reg.") ///
  stats(N ate_est ate_true att_true catt_true, ///
        labels("\$N\$" "\$\widehat{\text{ATE}}\$" "True ATE" "True ATT" "True CATT") ///
        fmt(0 %9.3f %9.3f %9.3f %9.3f))


