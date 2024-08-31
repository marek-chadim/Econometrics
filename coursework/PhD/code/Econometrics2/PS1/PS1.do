*1.5
clear all
set seed 42
set obs 100
gen x = cond(_n <= 50, 1, cond(_n <= 75, 2, 3))
gen y0 = runiform()
gen y1 = x * runiform()
gen d = runiform() < .5
gen tau = y1 - y0
*1.6
tw (kdensity tau if x == 1) (kdensity tau if x == 2)   ///
	(kdensity tau if x == 3) , xtitle("{&tau}") ///
	legend(order(1 "x = 1" 2 "x = 2" 3 "x = 3") rows(1) position(6))
	graph export tau.png, replace
*1.7
gen y = y1 * d + y0 * (1 - d)
reg y d
scalar ate = _b[d]
reg y d if x == 1
scalar att1 = _b[d]
reg y d if x == 2
scalar att2 = _b[d]
reg y d if x == 3
scalar att3 = _b[d]
gen tau_d = y1 - y0 if d==1
gen tau1 = tau if x == 1
gen tau2 = tau if x == 2
gen tau3 = tau if x == 3
egen tau_mean = mean(tau)
scalar tau_mean = tau_mean
egen tau1_mean = mean(tau1)
scalar tau1_mean = tau1_mean
egen tau2_mean = mean(tau2)
scalar tau2_mean = tau2_mean
egen tau3_mean = mean(tau3)
scalar tau3_mean = tau3_mean
egen tau_d_mean = mean(tau_d)
scalar tau_d_mean = tau_d_mean
matrix table = (ate, ate, att1, att2, att3) \ (tau_mean, tau_d_mean, tau1_mean, tau2_mean, tau3_mean)
matrix list table
*2f
clear all
set seed 42
set obs 200
local p = .3
local alpha0 = 1
local alpha1 = -2
gen y0 = rnormal()
gen x = runiform() < `p'
gen d = runiform() < .5
gen tau_i = `alpha0' + `alpha1'*x
gen y = y0 + d*tau_i
preserve
collapse y, by(d)
gen tau_hat = y - y[_n-1]
list tau_hat 
restore
reg y d
*2g
preserve
collapse y, by(d x)
sort x d
gen tau_x_hat = y - y[_n-1]
list tau_x_hat if x == 0 & d == 1 
list tau_x_hat if x == 1 & d == 1 
restore
gen dx0 = d*(1-x)
gen dx1 = d*x
reg y dx0 dx1 x
*2h
cap program drop sim
program define sim, rclass
	syntax [, obs(integer 1) exper(integer 1) p(real .3) pd_exper1(real 0.5)]
	clear
	set obs `obs'
	gen y0 = rnormal()
	local p = `p' 
	local q = `p'/(1-`p') 
	gen u = (1+`q')*runiform()
	sort u
	gen x = _n >= (1 - `p') * `obs' + 1
	if `exper' == 1 {
		gen d = runiform() <= `pd_exper1' 
	}
	else if `exper' == 2 {
		gen order_r = runiform()
		sort x order_r
		gen d = _n <= (`p' * `obs') | _n >= (1 - `p') * `obs' + (`p' * `obs')/2 + 1
	}
	local alpha0 = 1
	local alpha1 = -2
	gen tau_i = `alpha0' + `alpha1'*x
	gen y = y0 + d*tau_i
	reg y d
	return scalar tau = _b[d]
	gen dx0 = d*(1-x)
	gen dx1 = d*x
	reg y dx0 dx1 x
	return scalar tau0 = _b[dx0]
	return scalar tau1 = _b[dx1]
	
end
forval k = 1/2 {
	simulate tau_exp`k'=r(tau) tau0_exp`k'=r(tau0) tau1_exp`k'=r(tau1), ///
		reps(500) seed(42): sim, ///
		obs(100) exper(`k') p(0.3) 
		
	gen id = _n
	tempfile est`k'
	save `est`k''
}
use `est1', clear
merge 1:1 id using `est2'
tw (kdensity tau_exp1, xline(.4)) (kdensity tau_exp2, xline(.4)), ///
	legend(order(1 "Experiment 1" 2 "Experiment 2") rows(1) position(6))
graph export att.png, replace
tw (kdensity tau0_exp1, xline(1)) (kdensity tau0_exp2, xline(-1)) ///
	(kdensity tau1_exp1, xline(1)) (kdensity tau1_exp2, xline(-1)), ///
	legend(order(1 "x = 0 (Experiment 1)" 2 "x = 0 (Experiment 2)" 3 "x = 1 (Experiment 1)" 4 "x = 1 (Experiment 2)") rows(2) position(6))
graph export catt.png, replace
*3d 
clear all
set seed 42
set obs 100
matrix C = (1, 0 \ 0, 1)
drawnorm y1 y0, means(2, 1) cov(C)
gen random = runiform()
sort random
gen d = 0
replace d = 1 in 1/50
gen tau = y1 - y0 
quiet sum tau 
scalar mean = r(mean) 
gen y = y1 * d + y0 * (1 - d)
quiet reg y d 
scalar d = _b[d]
kdensity tau, xtitle("{&tau}")
graph export satt.png, replace
*3e
keep y d
reg y d 
scalar tau_hat = _b[d]  
save data, replace 
cap program drop tastingtea
program define tastingtea
	cap drop d 
	cap drop random 
	gen random = runiform()
	sort random 
	gen d = 0
	replace d = 1 in 1/50
	quiet reg y d 
end
simulate _b, reps(100): tastingtea
count if _b_d > `=tau_hat'
hist _b_d, xtitle("{&tau}-hat") xlabel(0 0.5 1 1.5) xline(`=tau_hat')
graph export tastingtea.png, replace
*3f
cap program drop bootstrap 
program define bootstrap 
	use data, clear
	bsample
	quiet reg y d
end
simulate _b, reps(1000): bootstrap 
count if _b_d < 0
hist _b_d, xtitle("{&tau}-hat") xlabel(0 0.5 1 1.5) xline(`=tau_hat') 
graph export bootstrap.png, replace
