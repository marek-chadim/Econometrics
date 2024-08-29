/*-----------------------------------------------------------------------------*
Marek Chadim
mach5689@student.su.se
Econometrics 2
Problem Set 5
*-----------------------------------------------------------------------------*/
clear all 
set more off, permanently   
set linesize 255   
set scheme lean2                 
cap mkdir output
cap log close
cap mkdir log
cap log using log/ps5.log, replace
*-----------------------------------------------------------------------------*

*5
set seed 42
set obs 300
gen i = _n
gen g = cond(i <= 100, 2, cond(i <= 200, 3, 4)) 
expand 4 
bysort i: gen t = _n 
bysort i: gen alpha = uniform()
replace alpha = alpha[_n-1] if i == i[_n-1]
gen tau = 0
gen D = 0
foreach t of numlist 1/4 {
    replace D = (t >= g) if t == `t'
    replace tau = cond(t < g, 0, cond(t == g, ///
    g/2 + rnormal(0, sqrt(0.1)), tau[_n-1] - (t-g)/5 + rnormal(0, 1))) if t == `t'
}
gen Y = alpha + D * tau + rnormal(0, 1) 
replace Y = . if t < g - 1

*6
preserve
collapse Y, by(g t)
panelview Y, i(g) t(t) type(missing)
graph export "output/6.png", width(4000) replace
restore

*7
preserve
collapse (mean) Y, by(g t)
twoway (connected Y t if g==2) ///
	   (connected Y t if g==3) ///
	   (connected Y t if g==4), graphregion(color(white)) ///
	   legend(label(1 "G2") label(2 "G3") label(3 "G4"))
graph export "output/7.png", width(4000) replace
restore

*8
est clear
sum tau
scalar att = r(mean)
eststo: reghdfe Y D, absorb(i t) 
estadd scalar true_att = att

*9
sum tau if g==t
scalar attg= r(mean)
gen D0 = cond(g == t, 1, 0) 
gen D1= cond(g==t[_n-1], 1, 0) 
replace D1 = 0 if g==4 & t==1
eststo: reghdfe Y D0 D1, absorb(i t)
estadd scalar true_att = attg 
esttab using "output/89.tex", scalar("true_att True ATT") keep(D*) se nodepvar mtitles("TWFE" "DynamicDID") replace
log close
