*------------------------------------------------------------------------------*
/*-----------------------------------------------------------------------------*
Shuhei Kainuma
shuhei.kainuma@su.se
2024-04-30

Econometrics 2
Problem Set 3: Question 1

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
log using log/PS3_Q1.log, replace


set scheme lean2


scalar rseed = 30042024


* simulation

global beta_fs = 1
global beta_ss = 1


** generate 10,000 samples
cap program drop assignment_random
program define assignment_random, rclass
	clear
	set obs 20
	gen eps = 3 * rnormal()
	gen z = rnormal()		
	gen x = $beta_fs * z + eps + rnormal()
	gen y = $beta_ss * x + eps
	
	reg x z
	return scalar beta_FS = _b[z]
	return scalar rss_FS  = `e(rss)'
	test z = 0
	return scalar F_stat = `r(F)'	
	ivreg y (x = z)
	return scalar beta_IV1 = _b[x]	
	
	gen xp = $beta_fs * z
	reg y xp
	return scalar beta_IV2 = _b[xp]	
	gen ess = (x - xp)^2
	sum ess 
	return scalar ess_FS = `r(N)' * `r(mean)'

	drop eps z x y xp
end
	
simulate rss_fs=r(rss_FS) ess_fs=r(ess_FS) f_stat=r(F_stat) ///
	beta_fs=r(beta_FS) beta_iv1=r(beta_IV1) beta_iv2=r(beta_IV2), ///
	reps(10000) seed(`=rseed'): assignment_random
	
save output/ps3_q1_data, replace 




local rss_fs_label    "FS Residual Sum Of Squares"
local ess_fs_label    "FS Error Sum Of Squares"
local rssess_fs_label "FS Residual over Error Sum Of Squares"
local f_stat_label    "First Stage F-Statistic"
local beta_fs_label   "First Stage Coefficient Estimate"
local beta_iv1_label  "Second Stage Coefficient Estimate (2SLS)"			
local beta_iv2_label  "Second Stage Coefficient Estimate (Imposing True First Stage)"
	



* 1.a ==========================================

sum beta_iv1
local mean_beta_iv1 = `r(mean)'
sum beta_iv2
local mean_beta_iv2 = `r(mean)'

twoway (kdensity beta_iv1, lc(red)) (kdensity beta_iv2, lc(blue)) ///
	, xtitle("") ///
	xline(`mean_beta_iv1', lp(shortdash) lc(red%30)) ///
	xline(`mean_beta_iv2', lp(shortdash) lc(blue%30)) ///
	legend(order(1 "{&beta} (2SLS)" 3 "Mean: `=round(`mean_beta_iv1',0.001)'" ///
		     2 "{&beta} (Imposing true FS)" 4 "Mean: `=round(`mean_beta_iv2',0.001)'") ring(0) position(2))
graph export "output/Fig_q1_a_1.png", replace

set graphics off 
	cap kdensity beta_iv1, gen(x1 d1)
	cap kdensity beta_iv2, gen(x2 d2)
set graphics on

twoway (line d1 x1 if inrange(x1,-10,10), lc(red)) (line d2 x2 if inrange(x2,-10,10), lc(blue)) ///
	, ///
	xline(`mean_beta_iv1', lp(shortdash) lc(red%30)) ///
	xline(`mean_beta_iv2', lp(shortdash) lc(blue%30)) ///
	legend(order(1 "{&beta} (2SLS)" 3 "Mean: `=round(`mean_beta_iv1',0.001)'" ///
		     2 "{&beta} (Imposing true FS)" 4 "Mean: `=round(`mean_beta_iv2',0.001)'") ring(0) position(11))
graph export "output/Fig_q1_a_2.png", replace

twoway (kdensity beta_iv1 if inrange(beta_iv1,-10,10), lc(red)) (kdensity beta_iv2 if inrange(beta_iv2,-10,10), lc(blue))  ///
	, xtitle("") ///
	xline(`mean_beta_iv1', lp(shortdash) lc(red%30)) ///
	xline(`mean_beta_iv2', lp(shortdash) lc(blue%30)) ///
	legend(order(1 "{&beta} (2SLS)" 3 "Mean: `=round(`mean_beta_iv1',0.001)'" ///
		     2 "{&beta} (Imposing true FS)" 4 "Mean: `=round(`mean_beta_iv2',0.001)'") ring(0) position(11))
graph export "output/Fig_q1_a_3.png", replace




* 1.b ==========================================

gen bias = abs(beta_iv1 - 1)	
local bias_label "Absolute Second Stage Bias"	

sum bias if beta_fs > $beta_fs
dis "Mean `bias_label' when beta_fs > 1 = `r(mean)'"
sum bias if beta_fs < $beta_fs
dis "Mean `bias_label' when beta_fs < 1 = `r(mean)'"

reg bias f_stat if f_stat > 10

binscatter bias f_stat, n(50) ytitle("Absolute second stage bias") xtitle("First stage F-statistic")
graph export "output/Fig_q1_b_all.png", replace
 
binscatter bias f_stat if f_stat > 10, n(50) ytitle("Absolute second stage bias") xtitle("First stage F-statistic")
graph export "output/Fig_q1_b_above10.png", replace



* 1.c ==========================================

gen ratio_rss_ess = rss_fs/ess_fs
local ratio_rss_ess_label "Residual over Error Sum-Of-Square"

binscatter rss_fs beta_fs, n(50) line(none) ///
	ytitle("`rss_fs_label'") ylabel(160(20)260) ///
	xtitle("") xlabel(none) name(rss_beta, replace) ///saving(output/rss_beta.gph, replace) ///
	plotregion(color(white)) xline($beta_fs, lc(orange)) scale(0.8)
	
binscatter ess_fs beta_fs, n(50) line(none) ///
	ytitle("`ess_fs_label'") ylabel(160(20)260) ///
	xtitle("") xlabel(none) name(ess_beta, replace) ///
	plotregion(color(white)) xline($beta_fs, lc(orange)) scale(0.8)
	
binscatter ratio_rss_ess beta_fs, n(50) line(none) ///
	ytitle("`rssess_fs_label'") xtitle("`beta_fs_label'") ///
	name(rssess_beta, replace) ///
	plotregion(color(white)) xline($beta_fs, lc(orange)) scale(0.8)			   

graph combine rss_beta ess_beta rssess_beta, ///
  cols(1) xcommon ysize(1.5) xsize(1)
graph export "output/Fig_q1_c.png", replace

sum ratio_rss_ess
dis "Max RSS/ESS Ratio = `r(max)'"



* 1.d (1) ==========================================

binscatter bias beta_fs, n(50) line(none) ///
	ytitle("`bias_label'") xtitle("") xlabel(none) ///
	name(bias_beta, replace) ///
	plotregion(color(white)) xline($beta_fs, lc(orange)) yline(0, lp(dash))scale(0.8)

binscatter f_stat beta_fs, n(50) line(none) ///
	ytitle("`f_stat_label'") xtitle("`beta_fs_label'") ///
	name(f_stat_beta, replace) ///
	plotregion(color(white)) xline($beta_fs, lc(orange)) yline(0, lp(dash))scale(0.8)
							   	
graph combine bias_beta f_stat_beta, cols(1) xcommon ysize(1) xsize(1)
graph export "output/Fig_q1_d.png", replace
	
qui sum bias
sum beta_fs if bias == `r(max)'
dis "beta_FS for the largest absolute bias = `r(mean)'"


* 1.d (2) ==========================================

binscatter bias beta_fs if beta_fs > 0.5, n(50) ylabel(0(.2)1) line(none) ///
	name(bias_beta_2, replace) ///
	xtitle("") xlabel(none) ytitle("`bias_label'") ///
	plotregion(color(white)) yline(0, lp(dash)) xline($beta_fs, lc(orange)) scale(0.8)

binscatter f_stat beta_fs if beta_fs > 0.5, n(50) line(none) ///
	name(f_stat_beta_2, replace) ///
	xtitle("`beta_fs_label'") ytitle("`f_stat_label'") ///
	plotregion(color(white)) xline($beta_fs, lc(orange)) yline(0, lp(dash)) scale(0.8)

graph combine bias_beta_2 f_stat_beta_2, cols(1) xcommon ysize(1) xsize(1)
graph export "output/Fig_q1_d_above05.png", replace

egen bias_min = min(bias) if beta_fs > 0.5
sum beta_fs if bias == bias_min
dis "beta_FS for the smallest absolute bias = `r(mean)'"



log close