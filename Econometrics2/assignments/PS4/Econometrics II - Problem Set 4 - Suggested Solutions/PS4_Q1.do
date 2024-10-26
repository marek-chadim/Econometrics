*------------------------------------------------------------------------------*
/*-----------------------------------------------------------------------------*
Shuhei Kainuma
shuhei.kainuma@su.se
2024-05-08

Econometrics 2
Problem Set 4: Question 1

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
log using log/PS4_Q1.log, replace


set scheme lean2


set seed 508


* Q1.6 =======================================

* generate data
** villages
set obs 100 // villages

gen j = _n
gen d = runiform() > .5
gen mu_j = rnormal(2,1)

*** save for later use
save tmp/mu, replace


** individuals
expand 10 // individuals

bys j: gen i = _n

gen w = runiform() > .5

gen y00 = rnormal(0, 1)
gen y10 = rnormal(1, 1)
gen y01 = rnormal(mu_j, 1)


cap program drop sample_phi
program define sample_phi, 
  syntax[, phi(real 0)]

  keep j - y01  
  gen x = rnormal(`phi', 0.2)
  gen y11 = y10 + y01 + x

  gen y = (1-d)*(1-w)*y00 ///
    + d*(1-w)*y10 ///
    + (1-d)*w*y01 ///
    + d*w*y11

end


forvalues phi = -1(.2)1 {
	
  sample_phi, phi(`phi')
	reg y d w, cl(j)
	
	mat d_coef_s1 = [nullmat(d_coef_s1) \ _b[d]]
	mat d_se_s1 = [nullmat(d_se_s1) \ _se[d]]
	mat w_coef_s1 = [nullmat(w_coef_s1) \ _b[w]]
	mat w_se_s1 = [nullmat(w_se_s1) \ _se[w]]

	gen dw = d*w
	reg y d w dw, cl(j)
	
	mat d_coef_s2 = [nullmat(d_coef_s2) \ _b[d]]
	mat d_se_s2 = [nullmat(d_se_s2) \ _se[d]]
	mat w_coef_s2 = [nullmat(w_coef_s2) \ _b[w]]
	mat w_se_s2 = [nullmat(w_se_s2) \ _se[w]]
	
	mat phis = [nullmat(phis) \ round(`phi', .1)]
	
}

forval k = 1/2 {
	foreach var in d w {
		mat lb_`var'_s`k' = `var'_coef_s`k' - 1.96*`var'_se_s`k'
		mat ub_`var'_s`k' = `var'_coef_s`k' + 1.96*`var'_se_s`k'
	}
}


forval k = 1/2 {
	foreach var in d w {
		svmat `var'_coef_s`k'
		svmat lb_`var'_s`k'
		svmat ub_`var'_s`k'
	}
}
svmat phis

* without interaction
twoway (rcap ub_d_s11 lb_d_s11 phis1, lcolor(navy)) ///
	(connected d_coef_s11 phis1, mcolor(navy) msymbol(O) lcolor(navy)) ///
	(rcap ub_w_s11 lb_w_s11 phis1, lcolor(orange)) ///
	(connected w_coef_s11 phis1, mcolor(orange) msymbol(S) lcolor(orange) yaxis(1 2)), ///
	yline(1, lcolor(navy%20)   axis(2)) ///
	yline(2, lcolor(orange%20)  axis(2)) ///
  legend(order(2 "`=ustrunescape("\u03C4\u0302")'{sub:d}" ///
               4 "`=ustrunescape("\u03C4\u0302")'{sub:w}") position(6) row(1)) ///
  xtitle("{&phi}") ylabel(1 "{&tau}{sub:d}" 2 "{&tau}{sub:w}")
graph export output/Fig_1_6_nointer.png, replace

* with interaction
twoway (rcap ub_d_s21 lb_d_s21 phis1, lcolor(navy)) ///
	(connected d_coef_s21 phis1, mcolor(navy) msymbol(O) lcolor(navy)) ///
	(rcap ub_w_s21 lb_w_s21 phis1, lcolor(orange)) ///
	(connected w_coef_s21 phis1, mcolor(orange) msymbol(S) lcolor(orange) yaxis(1 2)), ///
  ylabel(0.5(0.5)2.5, axis(2)) ///
	yline(1, lcolor(navy%20) axis(2)) ///
	yline(2, lcolor(orange%20) axis(2)) ///
  legend(order(2 "`=ustrunescape("\u03C4\u0302")'{sub:d}" ///
              4 "`=ustrunescape("\u03C4\u0302")'{sub:w}") position(6) row(1)) ///
  xtitle("{&phi}") ylabel(1 "{&tau}{sub:d}" 2 "{&tau}{sub:w}", axis(1))
graph export output/Fig_1_6_inter.png, replace




* Q1.7 =======================================

label variable d "\$D_j\$"
label variable w "\$W_{ij}\$"


eststo clear
forvalues i = 0/1 {
  qui sample_phi, phi(`i')

  * run regressions
  eststo: reg y d w, cl(j)
  eststo: reghdfe y w, absorb(j) cluster(j)
}


esttab using output/Tab_1_7.tex, ///
  cells(b(fmt(4)) se(par)) collabels(none) ///
  label replace keep(d w) ///
  mtitles("No Village FE" "Village FE" "No Village FE" "Village FE") ///
  mgroups("$\phi = 0$" "$\phi = 1$", pattern(1 0 1 0) ///
          prefix(\multicolumn{@span}{c}{) suffix(}) ///
                  span erepeat(\cmidrule(lr){@span})) ///
  substitute(\_ _)
	


* Q1.8 =======================================

qui sample_phi, phi(0)

gen tau_j = j*w
reghdfe y ib0.tau_j, absorb(j) cluster(j)

preserve
  regsave

  keep var coef
  rename coef tau_j

  gen j = regexs(1) if regexm(var, "([0-9]+).*")
  drop var

  destring j, replace

  drop if j < 1 | j > 100

  merge 1:1 j using tmp/mu, nogen

  sum mu_j, d
  twoway (scatter tau_j mu_j) ///
    (function y=x, range(`r(min)' `r(max)') lc(red%50)), ///
    xtitle("{&mu}{sub:j}") ytitle("`=ustrunescape("\u03C4\u0302")'{sub:j}") ///
    legend(order(2 "45-degree line") ring(0) position(11))
  graph export output/Fig_1_8.png, replace
	
restore	




* Q1.9 =======================================
set seed 1234
qui sample_phi, phi(0)
gen tau_j = j*w


local n_j = 10
local J = 100


** estimate sigma^2_Y by taking the within-village variance
qui reghdfe y, absorb(j tau_j)
scalar sigma2_y_hat = `e(rmse)'^2 


** estimate grand mean and sigma^2_mu
qui reghdfe y ib0.tau_j , absorb(j) cluster(j) resid
regsave //*j
rename coef tau_j_hat

drop if var == "0b.tau_j" | var == "_cons"
drop if ustrpos(var, "o")
gen j = subinstr(var, ".tau_j", "", .)
destring j, replace
keep j tau_j_hat

merge 1:1 j using tmp/mu, nogen // merge the true means to the estimates

sum tau_j_hat
scalar grandmean = `r(mean)'
dis "Grand mean: `=grandmean'"


qui reg tau_j_hat
scalar sigma2_0_hat = `e(rmse)'^2 - 4 * `=sigma2_y_hat'/`n_j'

scalar lambda = `=sigma2_0_hat'/(`=sigma2_0_hat' + 4 * `=sigma2_y_hat'/`n_j')
dis `=lambda'

gen mu_j_hat = `=lambda' * tau_j_hat + (1 - `=lambda') * `=grandmean'

egen mse_tau_j_hat = total(1/`n_j'*(tau_j_hat - mu_j)^2)
egen mse_mu_j_hat = total(1/`n_j'*(mu_j_hat - mu_j)^2)
sum mse*




* scatter
twoway (pcarrow tau_j_hat mu_j mu_j_hat mu_j, mc(black%50)lc(black%50)) ///
  (scatter tau_j_hat mu_j, m(o) mc(black%40) msize(0.8)) ///
  (pci -1.5 -1.5 5 5, lp(dash) lc(red%40)) ///
  (function y=`=grandmean', range(-2 5)), ///
  xlabel(-2(2)5) ylabel(-2(2)5) ///
  /// xscale(r(-2 5)) yscale(r(-2 5)) ///
  legend(order(1 "Shrinkage: `=ustrunescape("\u03C4\u0302")'{subscript:j} → {&mu}`=ustrunescape("\u0302")'{subscript:j}" ///y-bar{subscript:j} → {&mu}-hat{subscript:j}" ///
    2 "`=ustrunescape("\u03C4\u0302")'{subscript:j}" ///"y-bar{subscript:j}" ///
    3 "45-degree line" ///
    4 "Grand mean, `=ustrunescape("\u03C4\u0305")'") ///
    ring(0) position(11)) ///
  ytitle("`=ustrunescape("\u03C4\u0302")'{subscript:j} → {&mu}`=ustrunescape("\u0302")'{subscript:j}") xtitle("{&mu}{subscript:j}")
graph export output/Fig_1_9_1.png, replace


** order by mu_j - mu_j_hat
sort mu_j_hat
gen ordering = _n
twoway (pcarrow tau_j_hat ordering mu_j_hat ordering, mc(black%50)lc(black%50)) ///
  (function y=`=grandmean', range(0 100) lcolor(green%50)), ///
  legend(order(1 "Shrinkage: `=ustrunescape("\u03C4\u0302")'{subscript:j} → {&mu}`=ustrunescape("\u0302")'{subscript:j}" 2 "Grand mean, `=ustrunescape("\u03C4\u0305")'") ///
    ring(0) position(11)) ///
  ytitle("`=ustrunescape("\u03C4\u0302")'{subscript:j} → {&mu}`=ustrunescape("\u0302")'{subscript:j}") xtitle("Villages Ordered by `=ustrunescape("\u03C4\u0302")'{subscript:j}") xlabel("")
graph export output/Fig_1_9_2.png, replace



log close