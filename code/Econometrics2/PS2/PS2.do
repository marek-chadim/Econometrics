/*-----------------------------------------------------------------------------*
Marek Chadim
mach5689@student.su.se
Econometrics 2
Problem Set 2
*-----------------------------------------------------------------------------*/
clear all 
set more off, permanently   
set linesize 255    
set scheme lean2
set seed 42                        
cap mkdir output
cap log close
cap mkdir log
*-----------------------------------------------------------------------------*

*2b
clear all
cap log using log/q2.log, replace
set obs 100
gen e = rnormal(0, 1)
gen t = rnormal(1, 1)
gen x  = (_n > 50)
gen y0 = 5 * x 	+ e 
gen y1 = y0 + t
egen sate = mean(t)
scalar sate = sate
dis sate

*2c
cap program drop montecarloc 
program define montecarloc, rclass
gen random = runiform()
sort random
gen d = (_n/_N) <= .25
gen y = y1 * d + (1 - d) * y0 
reg y d
return scalar b = _b[d]
return scalar se = _se[d]
test _b[d] == `=sate'
return scalar p = r(p)
drop d y random
end	
preserve
simulate bc = r(b) sec = r(se) pc = r(p), reps(1000) saving(output/2c.dta, replace): montecarloc
restore

*2d
cap program drop montecarlod
program define montecarlod, rclass
gen random = runiform()
sort random
gen d = (_n/_N) <= .5
gen y = y1 * d + (1 - d) * y0 
reg y d
return scalar b = _b[d]
return scalar se = _se[d]
test _b[d] == `=sate'
return scalar p = r(p)
drop d y random
end	
preserve
simulate bd = r(b) sed = r(se) pd = r(p), reps(1000) saving(output/2d.dta, replace): montecarlod
restore

*2e
cap program drop montecarloe
program define montecarloe, rclass
gen random = runiform()
sort x random
by x: gen d = (_n/_N) <= .25
gen y = y1 * d + (1 - d) * y0 
reg y d
return scalar b = _b[d]
return scalar se = _se[d]
test _b[d] == `=sate'
return scalar p = r(p)
drop d y random
end	
preserve
simulate be = r(b) see = r(se) pe = r(p), reps(1000) saving(output/2e.dta, replace): montecarloe
restore

*2f
use output/2c.dta, clear
merge 1:1 _n using output/2d.dta, nogen
merge 1:1 _n using output/2e.dta, nogen
twoway (kdensity bc) (kdensity bd) (kdensity be), xline(1) legend(order(1 "c" 2 "d" 3 "e" )) xtitle("SATE")
graph export Output/2f.png, replace

*2g
gen n = _n
reshape long b se p, i(n) j(q) string
drop n
gen signif = p < 0.05
label variable b "coef"
label variable se "se"
label variable signif "p < .05"
foreach j in c d e { 
replace q = "(`w')" if q == "_`w'"
}
table () q, statistic(sd b) statistic(mean se signif) nototal  nformat(%9.3f)
cap log close

*3a
clear all
cap log using log/q3.log, replace
dis (1 + 5*(normal(-1)-normal(1)))
set obs 1000
gen e = rnormal(0, 1)
gen t = rnormal(1, 1)
egen sat = mean(t)
scalar sat = sat
dis sat
cap program drop montecarlo3
program define montecarlo3, rclass
	cap drop random d x* y*
	gen random = runiform()
	sort random
	gen d = (_n/_N) <= .5
	gen x0 	= e > -1
	gen x1 	= e > 1
	gen x  	= d * x1 + (1 - d) * x0 
	gen y0 	= 5 * x0 + e 
	gen y1 	= t + 5 * x1 + e
	gen y  	= d * y1 + (1 - d) * y0
	qui reg y d
	return scalar tau=_b[d]
	qui reg y d if x == 1
	return scalar tau1=_b[d]
	qui reg y d if x == 0
	return scalar tau0=_b[d]
end
montecarlo3
gen te = y1 - y0 
egen sate = mean(te)
scalar sate = sate
dis sate

*3bc
simulate tau = r(tau) tau1 = r(tau1) tau0 = r(tau0), reps(1000): montecarlo3
tabstat tau*, s(mean) columns(statistics)

*3d
dis (1 + normalden(1) / (1 - normal(1))) - (normalden(-1) / (1 - normal(-1)))
dis (1 - normalden(1) / normal(1)) + (normalden(-1) / normal(-1))
cap log close

*5a
clear all
cap log using log/q5.log, replace
set obs 10000
gen e = rnormal(0,1)
gen t = rnormal(1,1)
gen x  = (_n > 6000)
gen d = 0
replace d = 1 if x == 1 & runiform() < 0.8
replace d = 1 if x == 0 & runiform() < 0.5
gen y = t * d + 5 * x + e

*5bcd
eststo: reg y d if x == 0
eststo: reg y d if x == 1
eststo: reg y d x
eststo: reg y d
esttab, se
cap log close
