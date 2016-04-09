*---------------------------------
* Imm and L mkt outcomes
* Patcha Chaikitmongkol
*---------------------------------

clear

* Import cleaned data 
use "/Users/Mint/Dropbox/Dissertation_Data/cleanreg.dta"

gen yrqtr = yq(year,qtr)
egen panel_id = group(cwt skill)

* convert survey number from string to numeric
encode time_id, gen(time_id_num)

* specify panel data
xtset panel_id time_id_num

