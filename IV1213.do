* This code is for Iv regression

* Working Directory
cd "/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/R&R/IV"
use IVData0219.dta

/* 
encode VINCOME, gen(income_factor)
encode VSQFEET, gen(size_factor)
encode VHH_AGECODE, gen(age_factor)
gen ethnic_factor = 0 
replace ethnic_factor = 1 if VETHNIC == "White/Caucasian"
replace ethnic_factor = . if VETHNIC == ""
*/

* set panel data
xtset BILACCT_K Year      

* baseline model
xtreg CBP Heatpump Income Size Age Ethnic Dwelling_Age i.Year, r
est store CBP
xtreg CS Heatpump Income Size Age Ethnic Dwelling_Age i.Year, r
est store CS

* Fig 3 Heat pump effects with Regression results using thermal comfort indicators as the outcome variables 
coefplot CBP, bylabel(Cooling Balance Point) mcolor(red) ciopts(lc(red)) ///
|| CS, bylabel(Cooling Slope) mcolor(red) ciopts(lc(red)) ///
||, drop(_cons) xline(0) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f")) byopts(xrescale) ///
xtitle("{bf: Heat Pump Effects}") 


graph export "/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/R&R/IV/Baseline_HP022025.jpg", as(jpg) name("Graph") quality(100)

* IV
ivreg2 CBP Income Size Age Ethnic Dwelling_Age (Heatpump = HVAC_within_5km) i.Year, r first
est store CBP_5

ivreg2 CS Income Size Age Ethnic Dwelling_Age (Heatpump = HVAC_within_5km) i.Year, r first
est store CS_5

ivreg2 CBP Income Size Age Ethnic Dwelling_Age (Heatpump = HVAC_within_10km) i.Year, r first
est store CBP_10

ivreg2 CS Income Size Age Ethnic Dwelling_Age (Heatpump = HVAC_within_10km) i.Year, r first
est store CS_10

ivreg2 CBP Income Size Age Ethnic Dwelling_Age (Heatpump = HVAC_Nearest) i.Year, r first
est store CBP_Nearest

ivreg2 CS Income Size Age Ethnic Dwelling_Age (Heatpump = HVAC_Nearest) i.Year, r first
est store CS_Nearest


esttab CBP CS CBP_5 CS_5 CBP_10 CS_10 CBP_Nearest CS_Nearest using 022025IVResult.rtf


/* 
coefplot CBP_5, bylabel("Cooling Balance Point 5km") mcolor(red) ciopts(lc(red)) ///
|| CBP_10, bylabel("Cooling Balance Point 10km") mcolor(red) ciopts(lc(red)) ///
|| CBP_Nearest, bylabel("Cooling Balance Point Nearest") mcolor(red) ciopts(lc(red)) ///
||,drop(_cons) keep(Heatpump) xline(0) ///
mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f")) ///
byopts(rows(3) xrescale compact) ///
xtitle("{bf: Heat Pump Effects}") xscale(range(-15 0))

clear
set obs 6  // Number of models

gen model = ""
replace model = "CBP_5" in 1
replace model = "CS_5" in 2
replace model = "CBP_10" in 3
replace model = "CS_10" in 4
replace model = "CBP_Nearest" in 5
replace model = "CS_Nearest" in 6

gen heatpump_coef = .
gen heatpump_se = .

foreach m in CBP_5 CS_5 CBP_10 CS_10 CBP_Nearest CS_Nearest {
    estimates restore `m'
    replace heatpump_coef = _b[Heatpump] in `=_n'
    replace heatpump_se = _se[Heatpump] in `=_n'
}

gen lower_ci = heatpump_coef - 1.96 * heatpump_se
gen upper_ci = heatpump_coef + 1.96 * heatpump_se


twoway (scatter heatpump_coef model, msymbol(O)) ///
       (rcap lower_ci upper_ci model), ///
       ytitle("Heatpump Coefficient") xtitle("Model") ///
       title("IV Regression Results for Heatpump") ///
       xlabel(1 "CBP_5" 2 "CS_5" 3 "CBP_10" 4 "CS_10" 5 "CBP_Nearest" 6 "CS_Nearest") ///
       yline(0, lcolor(black) lpattern(dash))
