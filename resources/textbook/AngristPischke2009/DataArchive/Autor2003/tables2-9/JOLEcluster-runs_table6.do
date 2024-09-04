# delimit ;
set more 1;
set logtype text;
log using JOLEcluster-runs_table6, replace;
set matsize 450;
use autor-jole-2003.dta;


* Log total employment - from BLS employment & earnings;
gen lnemp=ln(annemp);

* Non-business-service sector employment from CBP;
gen nonemp= stateemp-svcemp;
gen lnnon=ln(nonemp);
gen svcfrac= svcemp/nonemp;

* Total business services employment from CBP;
gen bizemp= svcemp+peremp;
gen lnbiz = ln(biz);

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

*******************;
** Table 6
*******************;

/*By period*/;   

* 1 year intervals;
reg lnths lnemp mico mppa mgfa _Iyear* _Istate* _IstaXt_*  if year>=79 & year<=83, cluster(state);
reg lnths lnemp mico mppa mgfa _Iyear* _Istate* _IstaXt_*  if year>=83 & year<=87, cluster(state);
reg lnths lnemp mico mppa mgfa _Iyear* _Istate* _IstaXt_*  if year>=87 & year<=91, cluster(state);

* 2 year intervals - no linear trends;
reg lnths lnemp mico mppa mgfa _Iyear* _Istate*  if year>=79 & year<=83 & year2, cluster(state);
reg lnths lnemp mico mppa mgfa _Iyear* _Istate*  if year>=83 & year<=87 & year2, cluster(state);
reg lnths lnemp mico mppa mgfa _Iyear* _Istate*  if year>=87 & year<=91 & year2, cluster(state);

* 4 year intervals - no linear trends;
reg lnths lnemp mico mppa mgfa _Iyear* _Istate*   if year>=79 & year<=83 & year4, cluster(state);
reg lnths lnemp mico mppa mgfa _Iyear* _Istate*   if year>=83 & year<=87 & year4, cluster(state);
reg lnths lnemp mico mppa mgfa _Iyear* _Istate*   if year>=87 & year<=91 & year4, cluster(state);


/*By region*/;

* New England;
reg lnths lnemp mico mppa mgfa _Iyear* _Istate* _IstaXt_*  if region==1, cluster(state);

* Mid Atlantic;
reg lnths lnemp mico mppa mgfa _Iyear* _Istate* _IstaXt_*  if region==2, cluster(state);

* East North Central;
reg lnths lnemp mico mppa mgfa _Iyear* _Istate* _IstaXt_*  if region==3, cluster(state);

* West North Central;
reg lnths lnemp mico mppa mgfa _Iyear* _Istate* _IstaXt_*  if region==4, cluster(state);

* South Atlantic;
reg lnths lnemp mico mppa mgfa _Iyear* _Istate* _IstaXt_*  if region==5, cluster(state);

* East South Central;
reg lnths lnemp mico mppa mgfa _Iyear* _Istate* _IstaXt_*  if region==6, cluster(state);

* West South Central;
reg lnths lnemp mico mppa mgfa _Iyear* _Istate* _IstaXt_*  if region==7, cluster(state);

* Moutain Division;
reg lnths lnemp mico mppa mgfa _Iyear* _Istate* _IstaXt_*  if region==8, cluster(state);

* Pacific Division;
reg lnths lnemp mico mppa mgfa _Iyear* _Istate* _IstaXt_*  if region==9, cluster(state);

