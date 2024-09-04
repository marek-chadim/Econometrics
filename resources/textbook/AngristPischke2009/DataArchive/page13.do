/* create NHIS table on p. 13 of MHE */

/*

use phospy phstat using nhis2005pers
save nhis_13, replace

*/

use nhis_13, clear

keep if phstat <= 5 & phospyr  <= 2

gen health = 6 - phstat

ttest health, by(phospyr )