*------------------------------------------------------------------------------*
/*-----------------------------------------------------------------------------*
Shuhei Kainuma
shuhei.kainuma@su.se
2024-04-23

Econometrics 2
Problem Set 2: Question 5

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
log using log/PS2_Q5.log, replace


set scheme lean2




* 5.a ==========================================
set seed 2304
set obs 10000

** generate variables
gen x = (_n <= 4000)

gen tau = rnormal(1,1)
gen eps = rnormal(0,1)

gen d = 0
replace d = 1 if x == 1 & runiform() < 0.8
replace d = 1 if x == 0 & runiform() < 0.5

gen y = tau * d + 5 * x + eps // observed outcome

label variable y "\$Y_i\$"
label variable d "\$D_i\$"
label variable x "\$X_i\$"


* 5.b ==========================================
eststo clear

forvalues v = 0/1 {
  eststo: reg y d if x == `v'
}


* 5.c ==========================================

eststo: reg y d x


* 5.d ==========================================

eststo: reg y d


** create the table with the regression results
esttab using output/Tab_5.tex, ///
  se cells(b(fmt(4)) se(par)) collabels(none) ///
  mtitles("(b) \$X_i = 0\$" "(b) \$X_i = 1\$" "(c)" "(d)") ///
  label replace substitute(\_ _) nonumber note("Standard errors in parentheses.")



log close
