*------------------------------------------------------------------------------*
/*-----------------------------------------------------------------------------*
Stockholm School of Economics
2024-04-17

Econometric Game
Case A

NB: start Stata by opening this do file so that the working directory automatically
		becomes the directory in which the do file is stored.
*-----------------------------------------------------------------------------*/

*setup
clear all 
set more off, permanently  
set linesize 255          
version                 
cap mkdir output 
cap mkdir log 
cap log close
log using log/game.log, replace
set scheme lean2

/*
*data
import delimited using "Pisa201520182022_GameDataset.csv", clear
keep if oecd=="Yes"
gen math = (pv1math + pv2math + pv3math + pv4math + pv5math + pv6math + pv7math + pv8math + pv9math + pv10math)/10
gen read = (pv1read + pv2read + pv3read + pv4read + pv5read + pv6read + pv7read + pv8read + pv9read + pv10read)/10
save game.dta, replace
*/

* Part 1 ==========================================
use game.dta, replace
sum math read, d

**distribution
twoway (kdensity math if wave == 2015) ///
	(kdensity math if wave == 2018) ///
	(kdensity math if wave == 2022), ///
  xtitle("Math scores") ///
  ytitle("Density") ///
  legend(order(1 "2015" 2 "2018" 3 "2022") rows(1) position(6)) 
  graph export output/math_kdensity.png, replace
  
twoway (kdensity read if wave == 2015) ///
	(kdensity read if wave == 2018) ///
	(kdensity read if wave == 2022), ///
  xtitle("Reading scores") ///
  ytitle("Density") ///
  legend(order(1 "2015" 2 "2018" 3 "2022") rows(1) position(6)) 
  graph export output/read_kdensity.png, replace

**over time
bys wave: egen wavemath = mean(math)
bys wave: egen waveread = mean(read)
twoway (line wavemath wave) (line waveread wave) , title("Mean scores by wave") ytitle("Mean score") xtitle("Wave") legend(label(1 "Math") label(2 "Reading"))xlabel(2015 2018 2022) 
graph export output/scoresbywave.png, replace
  
**by status
bys cnt: egen escscnt = mean(escs)
egen escsgrp = cut(escscnt), group(3)
sum math read if escsgrp == 0
sum math read if escsgrp == 1
sum math read if escsgrp == 2

**by parenteduc
sum math read  if paredint < 15
sum math read  if paredint >= 15

**by gender
sum math read  if st004d01t == "Male"
sum math read  if st004d01t == "Female"

* Part 2 ==========================================
tab st021q01ta
encode st021q01ta, generate(startage)
bys wave cnt: egen startingage = mean(startage)

**score-starting age correlation 
bys wave cnt: egen mathcnt = mean(math)
corr mathcnt startage
bys wave cnt: egen readcnt = mean(read)
corr readcnt startage

**student characteristics controls
encode immig, generate(immigs)
encode st004d01t, gen(gender)
reg mathcnt startingage age grade immigs escs hisei paredint gender
reg readcnt startingage age grade immigs escs hisei paredint gender

*rule
label define month 1 "January" 2 "February" 3 "March" 4 "April" 5 "May" 6 "June" 7 "July" 8 "August" 9 "September" 10 "October" 11 "November" 12 "December"
encode st003d02t, generate(month) label(month) noextend
bys month cnt: egen startingagemonth = mean(startage)
label variable startingagemonth "Average Starting Age"
twoway (scatter startingagemonth month if !missing(month) ), by(cnt) xlabel(1(1)12) xtitle("Birth-Month")
graph export output/startingrule.png, replace

*Oosterbeek et al. (2021)
sum startage if cnt == "NLD" & month == 9
sum startage if cnt == "NLD" & month == 10
sum math read if cnt == "NLD" & month == 9
sum math read if cnt == "NLD" & month == 10
binscatter startage month if cnt == "NLD",xlabel(1(1)12)  line(lfit) rd(9) reportreg
graph export output/Oosterbeek.png, replace
binscatter math read month if cnt == "NLD", xlabel(1(1)12)  line(lfit) rd(9) reportreg

*empirics
encode cnt, generate(country)
mean startage, over(country month) 

*CZE#January
sum startage if cnt == "CZE" & month == 12
sum startage if cnt == "CZE" & month == 1

sum math read if cnt == "CZE" & month == 12
sum math read if cnt == "CZE" & month == 1
	
*LTU#January
sum startage if cnt == "LTU" & month == 12
sum startage if cnt == "LTU" & month == 1
sum math read if cnt == "LTU" & month == 12
sum math read if cnt == "LTU" & month == 1

sum startage  if (cnt == "CZE" | cnt == "LTU") & month == 12
sum startage  if (cnt == "CZE" | cnt == "LTU") & month == 1
sum math read if (cnt == "CZE" | cnt == "LTU") & month == 12
sum math read if (cnt == "CZE" | cnt == "LTU") & month == 1

*USA#Ferbruary
*SVN#February
*SVK#March
*TUR#March
*CHL#May
*CRI#May
*POL#July
*EST#September

*AUT#August
sum startage if cnt == "AUT" & month == 7
sum startage if cnt == "AUT" & month == 8
sum math read if cnt == "AUT" & month == 7
sum math read if cnt == "AUT" & month == 8

*HUN#August
sum startage if cnt == "HUN" & month == 7
sum startage if cnt == "HUN" & month == 8
sum math read if cnt == "HUN" & month == 7
sum math read if cnt == "HUN" & month == 8

*KOR#August
sum startage if cnt == "KOR" & month == 7
sum startage if cnt == "KOR" & month == 8
sum math read if cnt == "KOR" & month == 7
sum math read if cnt == "KOR" & month == 8

sum startage  if (cnt == "AUT" | cnt == "HUN" | cnt == "KOR" ) & month == 7
sum startage  if (cnt == "AUT" | cnt == "HUN" | cnt == "KOR" ) & month == 8
sum math read if (cnt == "AUT" | cnt == "HUN" | cnt == "KOR" ) & month == 7
sum math read if (cnt == "AUT" | cnt == "HUN" | cnt == "KOR" ) & month == 8

gen old=0
replace old=1 if month>=8
gen month_centered = month-8
reg startage old##c.month_centered, vce(robust)
reg math old##c.month_centered, vce(robust)

binscatter startage month if (cnt == "AUT" | cnt == "HUN" | cnt == "KOR" ) , line(lfit) rd(7) reportreg
graph export output/August.png, replace
binscatter math read month if (cnt == "AUT" | cnt == "HUN" | cnt == "KOR" ) , line(lfit) rd(7) reportreg
graph export output/Augustscores.png, replace