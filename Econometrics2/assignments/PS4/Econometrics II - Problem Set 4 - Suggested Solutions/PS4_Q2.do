*------------------------------------------------------------------------------*
/*-----------------------------------------------------------------------------*
Shuhei Kainuma
shuhei.kainuma@su.se
2024-05-08

Econometrics 2
Problem Set 4: Question 2

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
cap mkdir tmp

cap log close // Close a log-file (if there's one open).
log using log/PS4_Q2.log, replace


set scheme lean2


set seed 508


* Q2.5 =======================================

cap program drop generate_worker_machine
program define generate_worker_machine,
  syntax[, n_workers(real 10) n_machines(real 20) n_periods(real 200) mu_alpha(real 0) sigma_alpha(real 1) mu_psi(real 1) sigma_psi(real 1) mu_gamma(real 0) sigma_gamma(real 0.5) mu_eps(real 0) sigma_eps(real 0.2)]

  clear 

  * prepare machines dataset
  set obs `n_machines'
  gen j = _n
  gen psi = rnormal(`mu_psi', `sigma_psi')

  tempfile psi
  save `psi'

  * prepare period shocks datasets
  clear 
  set obs `n_periods'
  gen t = _n
  gen gamma = rnormal(`mu_gamma', `sigma_gamma')

  tempfile gamma
  save `gamma'

  * prepare worker dataset 
  clear 
  set obs `n_workers'
  gen i = _n
  gen alpha = rnormal(`mu_alpha', `sigma_alpha')

  expand `n_periods'
  bys i : gen t = _n
  
  bys i : gen j = runiformint(1, `n_machines')


  * merge in machines and periods
  merge m:1 j using `psi', gen(_merge_j)
  drop if _merge_j == 2 // = machines to which no worker is ever assigned => not identified

  merge m:1 t using `gamma', gen(_merge_t)

  drop _merge_*


  * generate output (perfect substitutes)
  gen y = gamma + alpha + psi + rnormal(`mu_eps', `sigma_eps')

end

generate_worker_machine //, mu_gamma(3)

* check which workers are assigned to at two different machines
sort i t
bys i: egen minj = min(j)
bys i: egen maxj = max(j)
count if minj == maxj


* Q2.7 =======================================
* estimate worker and machine effects
reghdfe y, a(gamma alpha_hat=alpha psi_hat=psi)

twoway (scatter alpha alpha_hat) (scatter psi psi_hat) (function y=x, range(-2 2)), ///
  ytitle("True Parameter Values") xtitle("Parameter Estimates") ///
  legend(order(1 "`=ustrunescape("\u03B1")' / `=ustrunescape("\u03B1\u0302")'" ///
               2 "`=ustrunescape("\u03A8")' / `=ustrunescape("\u03A8\u0302")'") ///
        ring(0) position(11)) ///
  title("T=200")
graph export output/Fig_2_7.png, replace

* count # workers/machines whose effects are not estimated
tab i if alpha_hat == .
tab j if psi_hat == .



* Q2.8 =======================================
* generate data with T = 5
generate_worker_machine, n_periods(5) //sigma_eps(3)

* check which workers are assigned to at two different machines
sort i t
bys i: egen minj = min(j)
bys i: egen maxj = max(j)
count if minj == maxj


* estimate worker and machine effects
reghdfe y, a(gamma alpha_hat=alpha psi_hat=psi)


twoway (scatter alpha alpha_hat) (scatter psi psi_hat) (function y=x, range(-2 2)), ///
  ytitle("True Parameter Values") xtitle("Parameter Estimates") ///
  legend(order(1 "`=ustrunescape("\u03B1")' / `=ustrunescape("\u03B1\u0302")'" ///
               2 "`=ustrunescape("\u03A8")' / `=ustrunescape("\u03A8\u0302")'") ///
        ring(0) position(11)) ///
  title("T=5")
graph export output/Fig_2_8.png, replace


* count # workers/machines whose effects are not estimated
tab i if alpha_hat == .
tab j if psi_hat == .

