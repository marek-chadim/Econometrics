
/*-----------------------------------------------------------------------------*
Marek Chadim, Oscar Nilsson Hallgren, Elias Ljungqvist, Hugo Ryberg 
42624, 42599, 42613, 42597 @student.hhs.se 
5321 Industrial Organization Spring 2024
Problem set 2
*-----------------------------------------------------------------------------*/
clear all
set more off, permanently   
set linesize 255   
*set scheme lean2                 
cap mkdir output
cap log close
cap mkdir log
cap log using log/ps2.log, replace
*-----------------------------------------------------------------------------*

* Load data
use Cars-1, clear

/* Description of the main variables in the dataset:

Data on all newly registered cars in five EU countries during a 30 year period, from 1970 to 1999. 

country1-5 are Belgium, France, Germany, Italy and the UK. 

co: car model in a given year and country. 

qu: number of new car registrations,

price: price of the car measured in 1000s of Euros in 1999 purchasing power,
 
horsepower: measured in kW, 

fuel: measured as fuel efficiency in liter per 100 km, 

width & height: in cm, 

brand: brand name of the model, 

domestic: dummy taking value 1 for domestically produced cars 

firm: name of the firm

pop: country population. 

segment: the car market is classified into five different segments: 
	subcompact, compact, intermediate, standard, and luxury.
*/

* ---------------------------------------------------------------------------- *

* 1) Summary statistics 

estpost tabstat domestic qu price horsepower fuel width height weight, c(stat) stat(mean sd min max n)     

* Output for LaTeX

#delimit ; //#delimit, with a ; at the end, allows to not have to ue the slashes and can use them to comment like this instead.
esttab using "output/SummaryStats.tex",
 replace ///Replace file if already exists
 cells("mean(fmt(3)) sd min max count(fmt(0))") ///Which Stats to Output
 nonumber ///Do not put numbers below column titles
 nomtitle ///This option mainly for regression tables
 booktabs ///Top, Mid, Bottom Rule
 noobs ///We don't need observation counts because count is N
 title("Summary Statistics\label{tab1}") ///Latex number this for us
 collabels("Mean" "SD" "Min" "Max" "N") /// Name of each column
 addnote("Source: Björnerstedt and Verboven's Cars-1 dataset.") ///Note below table
 coeflabels(year "Year" country "Country" qu "Number of new car regs" co "Car model" segment "Segment" domestic "1 if produced domestically" firm "Firm" brand "Brand" price "Price (in 1000 EUR)" horsepower "Horsepower (kW)" fuel "Fuel efficiency" width "Width (cm)" height "Height (cm)" weight "Weight (kg)") ///Label variables right in command
;

#delimit cr;

* ---------------------------------------------------------------------------- *

* Question 2) Hedonic Price Equation
egen yearcountry=group(year country)
xtset co yearcountry

* Generate the log of price
gen logprice = log(price)

* Reg it on characteristics
eststo clear
eststo: reg logprice horsepower fuel width height weight i.segment i.country domestic

* Output for LaTeX

#delimit ;
esttab using "output/RegTable1.tex", 
  se(3) /// SE with 3 Decimal Places
  b(3) /// Coefficients with 3 Decimal Places
  label
  replace /// Replace File
  title(Hedonic Price Regression \label{tab2}) /// Title of Tabel
  mtitles() /// Column Titles
  keep(horsepower fuel width height weight) /// Don't want all Control Coefficients
  coeflabels(horsepower "Horsepower (kW)" fuel "Fuel efficiency" width "Width (cm)" height "Height (cm)" weight "Weight (kg)" country1 "Prod. in Belgium" country2 "Prod. in France" country3 "Prod. in Germany" country4 "Prod. in Italy" country5 "Prod. in UK") /// Label Variables
;

#delimit cr;

* ---------------------------------------------------------------------------- *

* Question 3) Logit 

*share (s_j)
gen msize=pop/4
gen mshare=qu/msize
ge ln_mshare=log(mshare)
sum mshare ln_mshare

*segment market share 
egen tot_s=sum(qu), by(segment yearcountry)
ge mshare_s=qu/tot_s
ge ln_mshare_s=log(mshare_s)
sum mshare_s ln_mshare_s


* Outside option/good : 1 - sum(s_j) or 1 - (tot car regs/market size) 
egen tot_qu=sum(qu), by(yearcountry)
gen oshare=1-(tot_qu/msize) 

* Log of market share and the outside good share, i.e. log(s_j) & log(s_0)
* Difference between them, log(s_j) - log(s_0). LHS in our OLS
gen ln_dmshare=log(mshare)-log(oshare)

* OLS with the variables given in question. Country FEs are i.country where country1 (Belgium) is left out and Car model FEs are i.co. 
eststo clear
eststo: reg ln_dmshare price horsepower fuel width height domestic year i.country, r
*reghdfe ln_dmshare price horsepower fuel width height domestic, absorb(co yearcountry)
*xtreg ln_dmshare price horsepower fuel width height domestic year i.country, fe

* Extract the alpha 
gen alpha_OLS1=-_b[price]
*own price elasticitiy - Logit
gen e_jj_OLS=alpha_OLS1*(1-mshare)*price
sum e_jj_OLS


* Output for LaTeX
#delimit ;
esttab using "output/Logit.tex", 
  se(3) /// SE with 3 Decimal Places
  b(3) /// Coefficients with 3 Decimal Places
  label
  replace /// Replace File
  title(Logit \label{tab3}) /// Title of Tabel
  mtitles() /// Column Titles
  keep(price horsepower fuel width height domestic year _cons) /// Don't want all Control Coefficients
  coeflabels(price "Price (in 1000 EUR)" horsepower "Horsepower (kW)" fuel "Fuel efficiency" width "Width (cm)" height "Height (cm)" domestic "1 if produced domestically" year "Year") /// Label Variables
;

#delimit cr;

* ---------------------------------------------------------------------------- *

* Question 4) Nested Logit

* OLS with the segment market shares included
eststo clear
eststo: reg ln_dmshare price ln_mshare_s horsepower fuel width height domestic year i.country i.co
reghdfe ln_dmshare price ln_mshare_s horsepower fuel width height domestic, absorb(co yearcountry)


* Extract the alpha and the sigma 
gen alpha_OLS2=-_b[price]
gen sigma_OLS2=_b[ln_mshare_s]
ge e_jj_OLS2=-alpha_OLS2*(1/(1-sigma_OLS2)-(sigma_OLS2/(1-sigma_OLS2))*mshare_s-mshare)*price
sum e_jj_OLS2

* Output for LaTeX
#delimit ;
esttab using "output/NestedLogit.tex", 
  se(3) /// SE with 3 Decimal Places
  b(3) /// Coefficients with 3 Decimal Places
  label
  replace /// Replace File
  title(Nested Logit \label{tab4}) /// Title of Tabel
  mtitles() /// Column Titles
  keep(price ln_mshare_s horsepower fuel width height domestic year _cons) /// Don't want all Control Coefficients
  coeflabels(price "Price (in 1000 EUR)" horsepower "Horsepower (kW)" fuel "Fuel efficiency" width "Width (cm)" height "Height (cm)" domestic "1 if produced domestically" year "Year") /// Label Variables
;

#delimit cr;

* ---------------------------------------------------------------------------- *

* Question 6) Firm Averages: Marginal cost, Price & Lerner index

* Marginal Cost: Logit
gen MC_logit=price-1/(alpha_OLS1*(1-mshare))
bysort firm: summarize MC_logit if year==1998

* Marginal Cost: Nested Logit
gen MC_logit_n=price-(((1-sigma_OLS2)/alpha_OLS2)/(1-sigma_OLS2*mshare_s-(1-sigma_OLS2)*mshare))
bysort firm: summarize MC_logit_n if year==1998

* Average price
bysort firm: summarize price if year==1998


* Lerner Index: Logit
gen Lerner_logit = (price-MC_logit)/price
bysort firm: summarize Lerner_logit if year==1998

* Lerner Index: Nested Logit
gen Lerner_logit_n = (price-MC_logit_n)/price
bysort firm: summarize Lerner_logit_n if year==1998

* Output for LaTeX made with ChatGPT
sum MC_logit_n Lerner_logit_n price

* ---------------------------------------------------------------------------- *

* Question 7) VW/GM Merger simulation using the alpha_OLS2 and sigma_OLS2 values (needs to be entered numerically).

* STEP 1. initialize
mergersim init, nests(segment) price(price) quantity(qu) marketsize(msize) firm(firm)

* STEP 2. premerger investigation (i.co) not needed since already incl in xtset
xtreg M_ls price M_lsjg horsepower fuel width height domestic year country2-country5, fe
mergersim market if year == 1998 & country==3 

* STEP 3. merger simulation: GM (seller=15) and VW (buyer=26) in Germany 1998
mergersim simulate if year == 1998 & country==3, seller(15) buyer(26) detail

* Output for LaTeX made with ChatGPT

	// GRAPH PRICE EFFECTS

gen perc_price_ch=M_price_ch*100
graph bar (mean) perc_price_ch if country==3&year==1998, ///
	over(firm, sort(perc_price_ch) descending label(angle(vertical))) ///
	ytitle(Percentage) title(Average percentage price increase per firm) 
graph export output/a.png, replace	

* ---------------------------------------------------------------------------- *

* Question 8) Cost efficiencies 

* Updated STEP 3. with some cost efficiencies. 

mergersim simulate if year == 1998 & country==3, seller(15) buyer(26) ///
	sellereff(0.1) buyereff(0.1) method(fixedpoint) maxit(40) dampen(0.5) detail
	
* Output for LaTeX made with ChatGPT

// EFFICIENCIES
mergersim mre if year == 1998 & country == 3, seller(15) buyer(26)

	// GRAPH PRICE EFFECTS
gen perc_price_chb=M_price_ch*100
graph bar (mean) perc_price_chb if country==3&year==1998, ///
	over(firm, sort(perc_price_chb) descending label(angle(vertical))) ///
	ytitle(Percentage) title(Average percentage price increase per firm)
graph export output/b.png, replace
	
* ---------------------------------------------------------------------------- *

* Question 9) Merger with structural remedy 
* "Structural remedy" because we can imagine that VW only were alllowed to merge with GM IF they did not buy these 3 Opel models.

gen new_firm = firm

* Selling everything of GM to VW except for co = 164, 166 and 168
replace new_firm = 26 if new_firm == 15 & co != 164 & co != 166 & co != 168

* Variiable for efficiency where the merged entity here will be #26 and the remaining Opel models will be #15.
gen efficiency = 0
replace efficiency = 0.09 if new_firm ==26
replace efficiency = -0.02 if new_firm ==15

* Merger Simualtion
 
mergersim simulate if year == 1998 & country == 3, seller(15) buyer(26)

mergersim simulate if year == 1998 & country == 3, newfirm(new_firm) efficiencies(efficiency) method(fixedpoint) maxit(40) dampen(0.5) detail

	// GRAPH PRICE EFFECTS
gen perc_price_chc=M_price_ch*100
graph bar (mean) perc_price_chc if country==3&year==1998, ///
	over(firm, sort(perc_price_chc) descending label(angle(vertical))) ///
	ytitle(Percentage) title(Average percentage price increase per firm)
graph export output/c.png, replace
log close

* ---------------------------------------------------------------------------- *
