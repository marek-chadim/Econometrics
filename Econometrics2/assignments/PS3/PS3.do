/*-----------------------------------------------------------------------------*
Marek Chadim
mach5689@student.su.se
Econometrics 2, 2024
Problem Set 3
*-----------------------------------------------------------------------------*/
clear all 
set more off, permanently   
set scheme lean2
set linesize 255
set seed 42                        
cap mkdir output
cap log close
cap mkdir log
cap log using log/q1.log, replace
*-----------------------------------------------------------------------------
*a
cap program drop montecarlo
program define montecarlo, rclass
	clear
	set obs 20
	gen e = rnormal(0, sqrt(3))
	gen z = rnormal()
	gen u = rnormal()
	gen x = z + e + u
	gen y = x + e
	reg x z
	return scalar b1 = _b[z]
	return scalar rss  = `e(rss)'
	test _b[z] = 0
	return scalar F = `r(F)'	
	ivreg y (x = z)
	return scalar b2 = _b[x]	
	gen xtilde = z
	reg y xtilde
	return scalar b3 = _b[xtilde]	
	gen ess = (e+u)^2
	sum ess 
	return scalar ess = `r(N)' * `r(mean)'
	drop e z u x y xtilde
end
simulate rss=r(rss) ess=r(ess) F=r(F) b1=r(b1) b2=r(b2) b3=r(b3), seed(42) reps(10000): montecarlo
save output/q1, replace
use output/q1.dta
sum b2, d //mean .4375127
sum b3, d //mean 1.008521
twoway (kdensity b2) (kdensity b3), legend(order(1 "2SLSx" 2 "2SLSxtilde")) xtitle({&beta}hat) name(a1, replace)
twoway (kdensity b2 if b2>-6 & b2<6) (kdensity b3), xline(.4375127) xline(1.008521, lp(dash)) ///
legend(order(1 "mean .4375127" 2 "mean 1.008521")) xtitle(-6 < {&beta}hat < 6) name(a2, replace)
graph combine a1 a2, col(1)
graph export "output/a.png", replace

*b
gen absbias = abs(b2 - 1)	
sum absbias if b1 > 1 //mean .2243988
sum absbias if b1 < 1 //mean 2.838942
binscatter absbias F, n(50) name(b1, replace)
binscatter absbias F if F > 10, n(50) name(b2, replace) xtitle(F > 10)
graph combine b1 b2, col(1)
graph export "output/b.png", replace

*c
gen ssratio = rss/ess
sum ssratio  //max .999998
binscatter rss b1, n(50) name(c1, replace) xtitle("")
binscatter ess b1, n(50) name(c2, replace) xtitle("")
binscatter ssratio b1, n(50) name(c3, replace) xtitle({&beta}hat FS)
graph combine c1 c2 c3, col(1)
graph export "output/c.png", replace

*d
gen absbiasdesc = -absbias
sort absbiasdesc
list b1 in 1/5 //.000352
sort absbias
list b1 in 1/5 //.7315149 (1.049486)
binscatter absbias b1, n(50) name(d1, replace) xtitle("")
binscatter F b1, n(50) name(d2, replace) xtitle({&beta}hat FS)
binscatter absbias b1 if b1 > 0.5, n(50) name(d3, replace) xtitle("")
binscatter F b1 if b1 > 0.5, n(50) name(d4, replace) xtitle({&beta}hat FS > .5)							   	
graph combine d1 d3 d2 d4
graph export "output/d.png", replace























log close