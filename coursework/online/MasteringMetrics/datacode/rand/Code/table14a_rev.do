*** Metrics
*** Table 1.4, Panel A (Health-care use)
*** goal: master RAND replication do file
*** Created by Hellary Zhang on 03/29/2021

clear all
set more off

// set to directory where this do file is located
cd "/NHIS/Data/"

*******************************I. Clean dataset*********************************
* Import dataset
* Sample used in Table 1.4, Panel A: The sample used to construct this panel consists of all participants (regardless of age at enrollment) who did not initially refuse enrollment in a plan. See README.txt file for more details.
use person_years.dta, clear

*Merge Data on Hospital Visits
merge 1:1 person year using annual_spend.dta
keep if _m==3
drop _m

* Group plans into four types: 
* 1-Free, 2-Individual Deductible, 3-Cost Sharing (25%/50%), 4-Catostrophic (Fam Deductible) (95%/100%)*
gen plantype =.

replace plantype = 1 if plan==24
replace plantype = 2 if plan==1
replace plantype = 2 if plan==5
replace plantype = 4 if plan>=2 & plan<=4
replace plantype = 4 if plan>=6 & plan<=8
replace plantype = 3 if plan>=9 & plan<=23

*Generate Plan Dummies
gen plantype_1= (plantype==1)
gen plantype_2= (plantype==2)
gen plantype_3= (plantype==3)
gen plantype_4= (plantype==4)

*Generate Site Dummies
gen site1=(site==1)
gen site2=(site==2)
gen site3=(site==3)
gen site4=(site==4)
gen site5=(site==5)
gen site6=(site==6)

*Attempt to correlate year variable from annyal expenditures data to correct calendar year in order to adjust for inflation

gen expyear=indv_start_year+year-1

*Adjust for inflation.  This will only be an estimate since the annual expenditures data does not tell us what month they received specific services. 
*The CPI adjustment values below are based on the June CPI from 1991 (see table found at http://www.seattle.gov/financedepartment/cpi/historical.htm ).

gen out_inf=.
replace out_inf=outsum*3.07 if expyear==1973
replace out_inf=outsum*2.76 if expyear==1974
replace out_inf=outsum*2.53 if expyear==1975
replace	out_inf	=	outsum*	2.39	if	expyear	==1976
replace	out_inf	=	outsum*	2.24	if	expyear	==1977
replace	out_inf	=	outsum*	2.09	if	expyear	==1978
replace	out_inf	=	outsum*	1.88	if	expyear	==1979
replace	out_inf	=	outsum*	1.65	if	expyear	==1980
replace	out_inf	=	outsum*	1.5	if	expyear	==1981
replace	out_inf	=	outsum*	1.41	if	expyear	==1982
replace	out_inf	=	outsum*	1.37	if	expyear	==1983
replace	out_inf	=	outsum*	1.31	if	expyear	==1984
replace	out_inf	=	outsum*	1.27	if	expyear	==1985

gen inpdol_inf=.
replace inpdol_inf=inpdol*3.07 if expyear==1973
replace inpdol_inf=inpdol*2.76 if expyear==1974
replace inpdol_inf=inpdol*2.53 if expyear==1975
replace	inpdol_inf	=	inpdol*	2.39	if	expyear	==1976
replace	inpdol_inf	=	inpdol*	2.24	if	expyear	==1977
replace	inpdol_inf	=	inpdol*	2.09	if	expyear	==1978
replace	inpdol_inf	=	inpdol*	1.88	if	expyear	==1979
replace	inpdol_inf	=	inpdol*	1.65	if	expyear	==1980
replace	inpdol_inf	=	inpdol*	1.5	if	expyear	==1981
replace	inpdol_inf	=	inpdol*	1.41	if	expyear	==1982
replace	inpdol_inf	=	inpdol*	1.37	if	expyear	==1983
replace	inpdol_inf	=	inpdol*	1.31	if	expyear	==1984
replace	inpdol_inf	=	inpdol*	1.27	if	expyear	==1985

*Generate Total Spending Variable
gen tot_inf=inpdol_inf+out_inf

* Create famid variable for clustering standard errors
gen famid=substr(fam_identifier,3,.)
destring famid, ignore("A") replace
gen any_ins=(plantype==1|plantype==2|plantype==3)

* Label variables for rows of table 1.4, Panel A
la var ftf "Face-to-face visits"
la var out_inf "Outpatient expenses"
la var totadm "Hospital admissions"
la var inpdol_inf "Inpatient expenses"
la var tot_inf "Total expenses"

*************************II. Create Table 1.4, Panel A***************************
* Column 1: Create means for catastrophic mean
matrix means_sd = J(5, 2, .)
local row = 1

foreach var of varlist ftf out_inf totadm inpdol_inf tot_inf {
	summarize `var' if plantype == 4
	matrix means_sd[`row', 1] = r(mean)
	matrix means_sd[`row', 2] = r(sd)
	local row = `row'+1
}

matrix rownames means_sd = ftf out_inf totadm inpdol_inf tot_inf
matrix list means_sd

#d ;
frmttable using "table14a_rev.doc", statmat(means_sd) substat(1) varlabels sdec(4)
		   ctitle("", "Cata. mean") replace;
#d cr

* Create regression output
* Column 2: Deductible plan compared to catastrophic plan
matrix deduct_diff = J(5, 2, .)
local row = 1

foreach var of varlist ftf out_inf totadm inpdol_inf tot_inf {
	reg `var' plantype_1 plantype_2 plantype_3, cl(famid)
	matrix deduct_diff[`row', 1] = _b[plantype_2]
	matrix deduct_diff[`row', 2] = _se[plantype_2]
	local row = `row'+1
}


#d ;
frmttable using "table14a_rev.doc", statmat(deduct_diff) varlabels sdec(4)
		   ctitle("", "Deduct - cata.") substat(1) merge;
#d cr

* Column 3: Coinsurance plan compared to catastrophic plan
matrix coins_diff = J(5, 2, .)
local row = 1

foreach var of varlist ftf out_inf totadm inpdol_inf tot_inf {
	reg `var' plantype_1 plantype_2 plantype_3, cl(famid)
	matrix coins_diff[`row', 1] = _b[plantype_3]
	matrix coins_diff[`row', 2] = _se[plantype_3]
	local row = `row'+1
}

#d ;
frmttable using "table14a_rev.doc", statmat(coins_diff) varlabels sdec(4)
		   ctitle("", "Coins - cata") substat(1) merge;
#d cr


* Column 4: Coinsurance plan compared to catastrophic plan
matrix free_diff = J(5, 2, .)
local row = 1

foreach var of varlist ftf out_inf totadm inpdol_inf tot_inf{
	reg `var' plantype_1 plantype_2 plantype_3, cl(famid)
	matrix free_diff[`row', 1] = _b[plantype_1]
	matrix free_diff[`row', 2] = _se[plantype_1]
	local row = `row'+1
}

#d ;
frmttable using "table14a_rev.doc", statmat(free_diff) varlabels sdec(4)
		   ctitle("", "Free - cata.") substat(1) merge;
#d cr

* Column 5: Any insurance plan compared to catastrophic plan
matrix any_diff = J(5, 2, .)
local row = 1

foreach var of varlist ftf out_inf totadm inpdol_inf tot_inf {
	reg `var' any_ins, cl(famid)
	matrix any_diff[`row', 1] = _b[any_ins]
	matrix any_diff[`row', 2] = _se[any_ins]
	local row = `row'+1
}

#d ;
frmttable using "table14a_rev.doc", statmat(any_diff) varlabels sdec(4)
		   ctitle("", "Any - cata.") substat(1) merge;
#d cr
