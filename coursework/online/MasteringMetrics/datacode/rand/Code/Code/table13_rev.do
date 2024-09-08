*** Metrics
*** Table 1.3
*** goal: master RAND replication do file
*** Created by Hellary Zhang on 03/29/2021

* Install packages
findit frmttable // click on 'sg97_5' in search results window to install package needed for frmttable command

clear all
set more off

* set to directory where the data file is located
cd "C:\Users\chadi\OneDrive - Handelshögskolan i Stockholm\MasteringMetrics\rand\Data"

*******************************I. Clean dataset*********************************
* Import dataset
* Sample used in Table 1.3: The sample used to construct this table consists of adult participants (age 14 to 61 at time of enrollment) who did not initially refuse to enroll. See README.txt file for more details.
use rand_initial_sample_2.dta, clear

* Plan types:
/* 
	Plan type 1 = "Free plan"
	Plan type 2 = "Deductible plan"
	Plan type 3 = "Coinsurance plan"
	Plan type 4 = "Catastrophic plan" or "No Insurance"
*/
gen any_ins=(plantype==1|plantype==2|plantype==3)

* Create famid variable for clustering standard errors
gen famid=substr(fam_identifier,3,.)
destring famid, ignore("A") replace

* Ensure dummy variable for free plan does not give "0's" to anyone without a plan
replace plantype_1=. if plantype==.

* Label variables for rows of table 1.3
la var female "Female"
la var blackhisp "Nonwhite"
la var age "Age"
la var educper "Education"
la var income1cpi "Family Income"
la var hosp "Hospitalized last year"
la var ghindx "General health index"
la var cholest "Cholesterol (mg/dl)"
la var systol "Systolic blood pressure (mm Hg)"
la var mhi "Mental health index"

la var plantype "Number enrolled"

*****************************II. Create Table 1.3*******************************
* Create means for catastrophic plan
matrix means_sd = J(11, 2, .)
local row = 1

foreach var of varlist female blackhisp age educper income1cpi hosp ghindx cholest systol mhi {
	summarize `var' if plantype == 4
	matrix means_sd[`row', 1] = r(mean)
	matrix means_sd[`row', 2] = r(sd)
	local row = `row'+1
}

count if plantype_4 == 1
matrix means_sd[11, 1] = r(N)

matrix rownames means_sd = female blackhisp age educper income1cpi hosp ghindx cholest systol mhi plantype
matrix list means_sd

#d ;
frmttable using "table13_rev.doc", statmat(means_sd) substat(1) varlabels sdec(4)
		   ctitle("", "Cata. mean") replace;
#d cr

* Create regression output
* Column 2: Deductible plan compared to catastrophic plan
matrix deduct_diff = J(11, 2, .)
local row = 1

foreach var of varlist female blackhisp age educper income1cpi hosp ghindx cholest systol mhi {
	reg `var' plantype_1 plantype_2 plantype_3, cl(famid)
	matrix deduct_diff[`row', 1] = _b[plantype_2]
	matrix deduct_diff[`row', 2] = _se[plantype_2]
	local row = `row'+1
}
count if plantype_2 == 1
matrix deduct_diff[11, 1] = r(N)

#d ;
frmttable using "table13_rev.doc", statmat(deduct_diff) varlabels sdec(4)
		   ctitle("", "Deduct - cata.") substat(1) merge;
#d cr

* Column 3: Coinsurance plan compared to catastrophic plan
matrix coins_diff = J(11, 2, .)
local row = 1

foreach var of varlist female blackhisp age educper income1cpi hosp ghindx cholest systol mhi {
	reg `var' plantype_1 plantype_2 plantype_3, cl(famid)
	matrix coins_diff[`row', 1] = _b[plantype_3]
	matrix coins_diff[`row', 2] = _se[plantype_3]
	local row = `row'+1
}

count if plantype_3 == 1
matrix coins_diff[11, 1] = r(N)

#d ;
frmttable using "table13_rev.doc", statmat(coins_diff) varlabels sdec(4)
		   ctitle("", "Coins - cata") substat(1) merge;
#d cr

* Column 4: Coinsurance plan compared to catastrophic plan
matrix free_diff = J(11, 2, .)
local row = 1

foreach var of varlist female blackhisp age educper income1cpi hosp ghindx cholest systol mhi {
	reg `var' plantype_1 plantype_2 plantype_3, cl(famid)
	matrix free_diff[`row', 1] = _b[plantype_1]
	matrix free_diff[`row', 2] = _se[plantype_1]
	local row = `row'+1
}

count if plantype_1 == 1
matrix free_diff[11, 1] = r(N)

#d ;
frmttable using "table13_rev.doc", statmat(free_diff) varlabels sdec(4)
		   ctitle("", "Free - cata.") substat(1) merge;
#d cr

* Column 5: Any insurance plan compared to catastrophic plan
matrix any_diff = J(11, 2, .)
local row = 1

foreach var of varlist female blackhisp age educper income1cpi hosp ghindx cholest systol mhi {
	reg `var' any_ins, cl(famid)
	matrix any_diff[`row', 1] = _b[any_ins]
	matrix any_diff[`row', 2] = _se[any_ins]
	local row = `row'+1
}

count if any_ins == 1
matrix any_diff[11, 1] = r(N)

#d ;
frmttable using "table13_rev.doc", statmat(any_diff) varlabels sdec(4)
		   ctitle("", "Any - cata.") substat(1) merge;
#d cr
