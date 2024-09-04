# delimit ;
set more 1;
set logtype text;
cap log using JOLE-speccheck_table4, replace;
set matsize 450;
use eow-svcs.dta, clear;



* Weights;
egen empwt=mean(annemp) if year>=79 & year<=95, by(state);
if "`1'"!="wt" {; replace empwt=1; };

* Log total employment - from BLS employment & earnings;
gen lnemp=ln(annemp);

* Non-business-service sector employment from CBP;
gen nonemp= stateemp-svcemp;
gen lnnon=ln(nonemp);
gen svcfrac= svcemp/nonemp;

* Total business services employment from CBP;
gen bizemp= svcemp+peremp;
gen lnbiz = ln(bizemp);

* State dummies, year dummies, and state*time trends;
gen t=year-78;
gen t2=t^2;
drop if state==98;
xi i.state i.year i.state*t i.state*t2 i.region*i.year;
drop _Iyear_77 - _Iyear_79;

* Working sample;
gen year1 = year>=79 & year<=95;
keep if year1;
gen year2 = ((int(year/2)*2 + 1)==year);
gen year4 = (year==79 | year==83 | year==87 | year==91 | year==95);

* Generate more aggregate demos;
gen clp=clg+gtc;
gen a1624=m1619+m2024+f1619+f2024;
gen a2554=m2554+f2554;
gen a55up=m5564+m65up+f5564+f65up;
gen fem=f1619+f2024+f2554+f5564+f65up;
gen white=rs_wm+rs_wf;
gen black=rs_bm+rs_bf;
gen other=rs_om+rs_of;
gen married=marfem+marmale;

* Modify union variable so that:
  1 - We don't use interpolated data for 1979 & 1981
  2 - Turn from fraction into percent so that coefficient will be friendly size;
replace unmem=. if year==79 | year==81;
replace unmem=unmem*100;

* Alternative legal characterizations;
gen mcount = mico+mppa+mgfa;
gen many = min(mcount,1);
gen mone = mcount==1;
gen mtwo = mcount==2;
gen mthree = mcount==3;


****************;
** Table 4
****************;

reg lnths mico mppa mgfa _Iyear* _Istate* _IstaXt_* [aw=empwt], cluster(state);
reg lnths many _Iyear* _Istate* _IstaXt_* [aw=empwt], cluster(state);
reg lnths mcount _Iyear* _Istate* _IstaXt_* [aw=empwt], cluster(state);
reg lnths mone mtwo mthree _Iyear* _Istate* _IstaXt_* [aw=empwt], cluster(state);
reg lnths mico mppa mgfa many _Iyear* _Istate* _IstaXt_* [aw=empwt], cluster(state);
reg lnths mico mppa mcount _Iyear* _Istate* _IstaXt_* [aw=empwt], cluster(state);
reg lnths mico mppa mgfa mtwo mthree _Iyear* _Istate* _IstaXt_* [aw=empwt], cluster(state);

