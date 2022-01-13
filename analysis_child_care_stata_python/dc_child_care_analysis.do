*****************************************************************
***** SET UP
*****************************************************************

*** Settings
clear all
set more off
set type double, perm

*** Path macros
global data "./data"
global charts "./charts"


*****************************************************************
***** CREATE FRAMES & IMPORT DATA
*****************************************************************

*** Child development center data (opendata.dc.gov)
** Note [1]: The dataset is from 2019 and was downloaded in April 2021; it has since been updated (https://opendata.dc.gov/datasets/DCGIS::child-development-centers/about).
** Note [2]: Washington, DC no longer uses the quality tier system included in this dataset & analysis.
frame create cdcenters
frame change cdcenters
import delimited "$data\Child_Development_Centers.csv", encoding(UTF-8) bindquote(strict) clear

*** ACS population data (see Python script for API collection steps)
frame create acsdata
frame change acsdata
import delimited "$data\acs_ward_under5_pop.csv", clear


*****************************************************************
***** EXPLORATION & PROCESSING
*****************************************************************

frame change cdcenters

*** Tabulate child care centers by DC ward
tab ward

*** Tabulate child care centers by type
tab type

*** Tabulate child care centers by quality tier
tab tier_name
** Clean tier names, re-tabulate
replace tier_name = subinstr(tier_name, char(13), "", .)
tab tier_name

*** Capacity
** Calculate total licensed capacity for children under age 5
gen lic_cap_under5 = lic_cap_infants + lic_cap_pre_child + lic_cap_tod
** Exclude centers with no capacity for children under age 5
drop if lic_cap_under5 == 0
** Summarize
summ lic_cap_under5, det

*** Create DC Total ward group
expand 2, gen(expand)
replace ward = "DC Total" if expand == 1
drop expand

*** Create DC Total tier group
expand 2, gen(expand)
replace tier_name = "DC Total" if expand == 1
drop expand

*** Create tier order helper
label define tier_order 0 "DC Total" 1 "Gold" 2 "Silver" 3 "Bronze" 4 "Private - Not Tiered"
encode tier_name, gen(tier_num) label(tier_order)

*** Create frames for analyses
frame copy cdcenters analysis1
frame copy cdcenters analysis2
frame copy cdcenters analysis3


*****************************************************************
***** ANALYSIS 1: CAPACITY BY TIER
*****************************************************************

frame change analysis1

*** Drop ward total
drop if ward == "DC Total"

*** Create helper label for plot, including counts
bys tier_name: gen count_centers = _N
tostring count_centers, replace

gen tier_name_plot = tier_name + " (n = " + count_centers + ")"
replace tier_name_plot = subinstr(tier_name_plot, " - Not Tiered", "", .)

*** Box plot - capacity by tier
graph ///
	hbox lic_cap_under5 if tier_name != "DC Total", ///
	over(tier_name_plot, sort(tier_num)) ///
	title("Distribution of Licensed Child Care Capacity by Quality Tier", color(black) size(med)) ///
	graphregion(color(white)) ///
	ytitle("Licensed Capacity") ///
	ylabel(, glcolor(gs14) glwidth(.2)) ///
	caption("Note: Capacity includes children under age 5 only." "          Centers serving only school-age children are excluded.", size(vsmall))
graph export "$charts/box_cap_tier.jpg", as(jpg) replace

*** Test significance of capacity differences using regression
reg lic_cap_under5 i.tier_num

*** Prep data for plot
collapse (mean) lic_cap_under5, by(tier_name tier_num)

*** Store confidence intervals
gen lower = .
gen upper = .

forval i = 1/4{
	replace lower = lic_cap_under5 - invttail(e(df_r), 0.025) * _se[`i'.tier_num] if tier_num == `i'
	replace upper = lic_cap_under5 + invttail(e(df_r), 0.025) * _se[`i'.tier_num] if tier_num == `i'
}

*** Bar plot with error bars
separate lic_cap_under5, by(tier_num == 0)

graph twoway ///
	(bar lic_cap_under50 tier_num, barwidth(.8) color(navy)) ///
	(bar lic_cap_under51 tier_num, barwidth(.8) color(sand)) ///
	(rcap upper lower tier_num, color(maroon)), ///
	title("Licensed Child Care Center Average Capacity by Quality Tier", color(black) size(med)) ///
	ytitle("Average Capacity") ///
	ylabel(0(20)80, angle(horizontal) glcolor(gs14) glwidth(.2)) ///
	xtitle("Quality Tier") ///
	xlabel(0 "DC Total" 1 "Gold" 2 "Silver" 3 "Bronze" 4 "Private", labsize(small)) ///
	graphregion(color(white)) ///
	plotregion(margin(l=4 b=0)) ///
	legend(off) ///
	caption("Note: Capacity includes children under age 5 only." "          Centers serving only school-age children are excluded.", size(vsmall))
graph export "$charts/avg_cap_by_tier.jpg", as(jpg) replace


*****************************************************************
***** ANALYSIS 2: QUALITY BY WARD
*****************************************************************

frame change analysis2

*** Drop tier total
drop if tier_name == "DC Total"

*** Sum capacity and count centers by ward and tier
gen center_count = 1
collapse (sum) lic_cap_under5 (sum) center_count, by(ward tier_name tier_num)

*** Calculate proporiton of capacity & centers by ward and tier
foreach var of varlist lic_cap_under5 center_count{
	bys ward: egen pc_`var' = pc(`var')
}
la var pc_lic_cap_under5 "Percent of Capacity"
la var pc_center_count "Percent of Centers"

*** Visualize proportion of capacity by tier and ward
graph bar (asis) pc_lic_cap_under5, ///
	over(tier_name, sort(tier_num)) ///
	over(ward, label(labsize(small))) ///
	asyvars stack ///
	bar(1, color("176 141 87")) ///
	bar(2, color("255 215 0")) ///
	bar(3, color("250 170 205")) ///
	bar(4, color("192 192 192")) ///
	title("Licensed Child Care Capacity by Quality Tier and Ward", color(black) size(med)) ///
	graphregion(color(white)) ///
	legend(order(2 4 1 3) rows(1) size(small) symxsize(8)) ///
	caption("Note: Capacity includes children under age 5 only." "          Centers serving only school-age children are excluded.", size(vsmall))
graph export "$charts/prop_tier_ward_capacity.jpg", as(jpg) replace

*** Visualize proportion of centers by tier and ward
graph bar (asis) pc_center_count, ///
	over(tier_name, sort(tier_num)) ///
	over(ward, label(labsize(small))) ///
	asyvars stack ///
	bar(1, color("176 141 87")) ///
	bar(2, color("255 215 0")) ///
	bar(3, color("250 170 205")) ///
	bar(4, color("192 192 192")) ///
	title("Licensed Child Care Centers by Quality Tier and Ward", color(black) size(med)) ///
	graphregion(color(white)) ///
	legend(order(2 4 1 3) rows(1) size(small) symxsize(8)) ///
	caption("Note: Capacity includes children under age 5 only." "          Centers serving only school-age children are excluded.", size(vsmall))
graph export "$charts/prop_tier_ward_count.jpg", as(jpg) replace
	

*****************************************************************
***** ANALYSIS 3: RATIO CARE CAPACITY TO CHILD POPULATION
*****************************************************************

frame change analysis3

*** Drop tier total
drop if tier_name == "DC Total"

*** Collapse capacity at ward level
collapse (sum) lic_cap_under5, by(ward)

*
***** Prep ACS data
*** Change frame
frame change acsdata

*** Tostring ward
ren statelegislativedistrictuppercha ward
tostring ward, replace
replace ward = "Ward " + ward

*** Create DC total
expand 2, gen(expand)
replace ward = "DC Total" if expand == 1
collapse (sum) pop_under5, by(ward)

*** Prep for merge
tempfile acsdata
save `acsdata', replace
*****
*

*** Change frame, merge
frame change analysis3
merge 1:1 ward using `acsdata', assert(3) nogen

*** Calculate care capacity ratio
gen ccratio = lic_cap_under5 / pop_under5

*** Visualize
graph bar (asis) ccratio, ///
	over(ward, label(labsize(small))) ///
	title("Ratio of Licensed Care Capacity to Child Population", color(black) size(med)) ///
	ytitle("Care Capacity Ratio") ///
	ylabel(, angle(horizontal) format(%4.2fc) glcolor(gs14) glwidth(.2)) ///
	graphregion(color(white)) ///
	plotregion(margin(l=4 b=0)) ///
	legend(off) ///
	caption("Note: Capacity includes children under age 5 only." "          Centers serving only school-age children are excluded.", size(vsmall))
graph export "$charts/ccratio_by_ward.jpg", as(jpg) replace


