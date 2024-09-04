# delimit ;
set more 1;
set logtype text;
log using HM-Union_table8, replace;
set matsize 450;
set mem 20m;
use eow-svcs.dta;


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
gen lnbiz = ln(biz);

* Add Hirsch-Macpherson-Vroman union data;
sort state year;
merge state year using hm-union-6400.dta;
drop if state==53;
assert _merge==3 if year>=79 & year<=95;
xi: reg unmem hm_union i.state i.year;
keep if year>=79 & year<=95;

* State dummies, year dummies, and state*time trends;
gen t=year-78;
gen t2=t^2;
drop if state==98;
xi i.state i.year i.state*t i.state*t2 i.region*i.year;


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


****************************;
** Table 8: unionization
****************************;

reg lnths hm_union _Iyear* _Istat* [aw=empwt] if year>=79, cluster(state);
reg lnths hm_union inagri-inmiss _Iyear* _Istat* _IstaXt_* [aw=empwt] if year>=79, cluster(state);
reg lnths hm_union mico mppa mgfa _Iyear* _Istat* _IstaXt_* [aw=empwt] if year>=79, cluster(state);
reg lnths hm_union lnemp mico mppa mgfa _Iyear* _Istat* _IstaXt_* [aw=empwt] if year>=79, cluster(state);
reg lnths hm_union lnemp mico mppa mgfa inagri-inmiss _Iyear* _Istat* _IstaXt_* [aw=empwt] if year>=79, cluster(state);
reg lnths lnemp hm_union mico mppa mgfa inagri-inmiss hsd smc clp black other a1624 a55up fem marfem married _Iyear* _Istat* _IstaXt_* _IstaXt2_*  _Ir* [aw=empwt] if year>=79, cluster(state);

