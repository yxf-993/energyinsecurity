use "/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data/Reg_HP_0421.dta"
encode VINCOME, gen(income_factor)
encode VSQFEET, gen(size_factor)
encode VHH_AGECODE, gen(age_factor)
gen ethnic_factor = 0
replace ethnic_factor = 1 if VETHNIC == "White/Caucasian"
xtset BILACCT_K V8
xtreg CBP HP income_factor size_factor age_factor ethnic_factor VRESAGE i.V8, re r
est store CBP
xtreg S3 HP income_factor size_factor age_factor ethnic_factor VRESAGE i.V8, re r
est store CS




coefplot CBP, bylabel(Cooling Balance Point) mcolor(red) ciopts(lc(red)) ///
|| CS, bylabel(Cooling Slope) mcolor(red) ciopts(lc(red)) ///
||, drop(_cons) xline(0) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f")) byopts(xrescale) ///
xtitle("{bf: Fig.2 Heat Pump Effects}") 


graph export "/Users/yexiaofeng/Desktop/0509_HP.png", as(png) name("Graph")

esttab CBP CS using 042124HP.rtf
esttab CBP CS using 042124HP.rtf




