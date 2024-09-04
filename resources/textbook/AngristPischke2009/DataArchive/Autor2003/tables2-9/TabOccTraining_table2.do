# delimit ;
set more 1;
set logtype text;
log using TabOccTraining_table2, replace;
set mem 25m;
use tempshare-occ8090;
use tempshare-occ8090;
merge occ8090 using trainfreq-occ8090;
keep if _merge==3;
replace tempshare=tempshare*100;



***********************;
* Table2: 3 digit occs
***********************;



summ [aw=occwt];

reg tempshare tktrnd [aw=occwt];
reg tempshare tenyr [aw=occwt];
reg tempshare tktrnd tenyr [aw=occwt];
reg tempshare typtrnsch tenyr [aw=occwt];
reg tempshare typtrnemp tenyr [aw=occwt];
reg tempshare typtrnojt tenyr [aw=occwt];
reg tempshare typtrnoth tenyr [aw=occwt];



