/*-----------------------------------------------------------------------------*
Marek Chadim
mach5689@student.su.se
Econometrics 2
Problem Set 4
*-----------------------------------------------------------------------------*/
clear all 
set more off, permanently   
set linesize 255   
set scheme lean2
cap mkdir data                     
cap mkdir output
cap log close
cap mkdir log
cap log using log/ps4.log, replace
*-----------------------------------------------------------------------------*

*1.5
set seed 42
set obs 100
gen village = _n
gen fiscal = runiform() > .5
gen casheffect = rnormal(2,1)
save data/villages, replace
expand 10
bys village: gen individual = _n
gen cash = runiform() > .5
gen po = rnormal(0, 1)
gen pofiscal = rnormal(1, 1)
gen pocash = rnormal(casheffect, 1)
cap program drop createoutcomes
program define createoutcomes, 
  syntax[, meaninteraction(real 0)]
  keep village - pocash  
  gen interaction = rnormal(`meaninteraction', 0.2)
  gen poboth = pofiscal + pocash + interaction
  gen fiscalcash = fiscal*cash
  gen outcome = (1-fiscal)*(1-cash)*po + fiscal*(1-cash)*pofiscal ///
  + (1-fiscal)*cash*pocash + fiscalcash*poboth
end
forval meaninteraction = -1(.2)1 {
	createoutcomes, meaninteraction(`meaninteraction')
	reg outcome fiscal cash, cl(village)
	mat bfiscal1 = [nullmat(bfiscal1) \ _b[fiscal]]
	mat sefiscal1 = [nullmat(sefiscal1) \ _se[fiscal]]
	mat bcash1 = [nullmat(bcash1) \ _b[cash]]
	mat secash1 = [nullmat(secash1) \ _se[cash]]
	reg outcome fiscal cash fiscalcash, cl(village)
	mat bfiscal2 = [nullmat(bfiscal2) \ _b[fiscal]]
	mat sefiscal2 = [nullmat(sefiscal2) \ _se[fiscal]]
	mat bcash2 = [nullmat(bcash2) \ _b[cash]]
	mat secash2 = [nullmat(secash2) \ _se[cash]]
	mat meaninteractions = [nullmat(meaninteractions) \ round(`meaninteraction', .1)]
}

*1.6
forval val = 1/2 {
	foreach var in fiscal cash {
		mat lower`var'`val' = b`var'`val' - 1.96*se`var'`val'
		mat upper`var'`val' = b`var'`val' + 1.96*se`var'`val'
	}
}
forval val = 1/2 {
	foreach var in fiscal cash {
		svmat b`var'`val'
		svmat lower`var'`val'
		svmat upper`var'`val'
	}
}
svmat meaninteractions
forval val = 1/2 {
twoway (rcap upperfiscal`val' lowerfiscal`val' meaninteractions) ///
(connected bfiscal`val' meaninteractions) (rcap uppercash`val' lowercash`val' meaninteractions) ///
(connected bcash`val' meaninteractions, yaxis(1 2)), yline(1, axis(2)) yline(2, axis(2)) ///
legend(order(1 "ATEhat fiscal" 2 "ATEhat cash") position(6) row(1)) ///
xtitle("mean interaction") ylabel(1 "ATE fiscal" 2 "ATE cash") name(ate`val', replace)
}
graph combine ate1 ate2
graph export output/6.png, replace 

*1.7
eststo clear
forval meaninteraction = 0/1 {
  qui createoutcomes, meaninteraction(`meaninteraction')
  eststo: reg outcome fiscal cash, cl(village)
  eststo: reghdfe outcome cash, absorb(village) cluster(village)
}
esttab using output/7.tex,  keep(fiscal cash) replace
	
*1.8
qui createoutcomes, meaninteraction(0)
gen cashvillage = cash*village
reghdfe outcome i.cashvillage, absorb(village) cluster(village) nocons
preserve
regsave
keep var coef
drop if var == "0b.cashvillage" 
gen village = subinstr(var, ".cashvillage", "", .)
destring village, replace
drop var
merge 1:1 village using data/villages, nogen
sum coef, d
twoway (scatter coef casheffect)
graph export output/8.png, replace
restore	

*1.9
sum cash
scalar cashpropensity = `r(mean)'
scalar cashvariance = `=cashpropensity' * (1-`=cashpropensity')
scalar villagers = 10
qui createoutcomes, meaninteraction(0)
gen cashvillage = cash*village
qui reghdfe outcome, absorb(village cashvillage)
scalar withinvar = `e(rmse)'^2 
qui reghdfe outcome i.cashvillage, absorb(village) cluster(village) nocons resid 
regsave 
keep var coef
drop if var == "0b.cashvillage" 
gen village = subinstr(var, ".cashvillage", "", .)
destring village, replace
drop var
merge 1:1 village using data/villages, nogen
sum coef, d
scalar prior = `r(mean)'
dis `=prior'
reg coef
scalar overdispersion = `e(rmse)'^2 - `=withinvar'/(`=villagers'*`=cashvariance')
scalar signaltonoise = `=overdispersion'/(`=overdispersion' +`=withinvar'/(`=villagers'*`=cashvariance'))
dis `=signaltonoise'
gen posterior = `=prior' + `=signaltonoise' * (coef-`=prior')
sum posterior
egen msecoef = total(1/10*(coef - casheffect)^2)
egen mseposterior = total(1/10*(posterior - casheffect)^2)
sum mse*
twoway (pcarrow coef casheffect posterior casheffect) ///
(scatter coef casheffect, m(x) msize(0.5)),  yline(`=prior') legend(position(6) row(1))
graph export output/9.png, replace
log close

*-----------------------------------------------------------------------------*/
clear all

*2.5
set seed 42
set obs 200
gen period = _n
gen gamma = rnormal(0, sqrt(.5))
expand 10
bys period: gen worker = _n
bys worker: gen alpha = rnormal(0,1)
replace alpha = alpha[_n-1] if worker == worker[_n-1]
gen machine = runiformint(1, 20)
bys machine: gen psi = rnormal(1,1) 
replace psi = psi[_n-1] if machine == machine[_n-1]
gen epsilon = rnormal(1, sqrt(.2))
gen output = alpha + gamma + psi + epsilon
save data/output, replace

*2.6
reghdfe output i.worker, absorb(period machine) nocons
regsave
drop if var == "1b.worker"
gen worker = subinstr(var, ".worker", "", .)
destring worker, replace
merge 1:m worker using data/output, nogen
twoway (scatter coef alpha) in 1/9, name(alpha6, replace)

use data/output, clear
reghdfe output i.machine, absorb(period worker) nocons
scalar obs=`e(df_m)'
regsave
drop if var == "1b.machine"
gen machine = subinstr(var, ".machine", "", .)
destring machine, replace
merge 1:m machine using data/output, nogen
twoway (scatter coef psi) in 1/`=obs', name(psi6, replace)
graph combine alpha6 psi6
graph export output/fe6.png, replace

*2.7
clear all
set seed 42
set obs 5
gen period = _n
gen gamma = rnormal(0, sqrt(.5))
expand 10
bys period: gen worker = _n
bys worker: gen alpha = rnormal(0,1)
replace alpha = alpha[_n-1] if worker == worker[_n-1]
gen machine = runiformint(1, 20)
bys machine: gen psi = rnormal(1,1) 
replace psi = psi[_n-1] if machine == machine[_n-1]
gen epsilon = rnormal(1, sqrt(.2))
gen output = alpha + gamma + psi + epsilon
save data/output2, replace

reghdfe output i.worker, absorb(period machine) nocons
regsave
drop if var == "1b.worker"
gen worker = subinstr(var, ".worker", "", .)
destring worker, replace
merge 1:m worker using data/output2, nogen
twoway (scatter coef alpha) in 1/9, name(alpha7, replace)

use data/output2, clear
reghdfe output i.machine, absorb(period worker) nocons
scalar obs=`e(df_m)'
regsave
drop if var == "1b.machine" 
gen machine = subinstr(var, ".machine", "", .)
destring machine, replace
sum machine
merge 1:m machine using data/output2, nogen
twoway (scatter coef psi) in 1/`=obs', name(psi7, replace)
graph combine alpha7 psi7
graph export output/fe7.png, replace







