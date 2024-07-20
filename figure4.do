cd /Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data
use Reg_HP_0421.dta,clear
encode VINCOME, gen(income_factor)
encode VSQFEET, gen(size_factor)
encode VHH_AGECODE, gen(age_factor)
gen ethnic_factor = 0
replace ethnic_factor = 1 if VETHNIC == "White/Caucasian"
xtset BILACCT_K V8
xtreg CBP HP size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==1, re r
est store m1
xtreg CBP HP size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==2, re r
est store m2
xtreg CBP HP size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==3, re r
est store m3
xtreg CBP HP size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==4, re r
est store m4
xtreg CBP HP size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==5, re r
est store m5
xtreg CBP HP size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==6, re r
est store m6
xtreg CBP HP size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==7, re r
est store m7
xtreg CBP HP size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==8, re r
est store m8
coefplot (m1, keep(HP) mc("0 114 178") lc("0 114 178")   ciopt(recast(ci) lcolor("0 114 178")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f"))) (m2,keep(HP) mc("0 114 178") lc("0 114 178")  ms(circle) ciopt(recast(ci) lcolor("0 114 178")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f")))  (m3, keep(HP) mc("0 114 178") lc("0 114 178")  ms(circle) ciopt(recast(ci) lcolor("0 114 178")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f"))) (m4,keep(HP) mc("0 114 178") lc("0 114 178")  ms(circle) ciopt(recast(ci) lcolor("0 114 178")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f")))  (m5, keep(HP) mc("0 114 178") lc("0 114 178")  ms(circle) ciopt(recast(ci) lcolor("0 114 178")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f"))) (m6,keep(HP) mc("0 114 178") lc("0 114 178")  ms(circle) ciopt(recast(ci) lcolor("0 114 178")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f"))) (m7, keep(HP) mc("0 114 178") lc("0 114 178")  ms(circle) ciopt(recast(ci) lcolor("0 114 178")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f"))) (m8,keep(HP) mc("0 114 178") lc("0 114 178") ms(circle) ciopt(recast(ci) lcolor("0 114 178")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f"))) , xline(0, lc(gs6) lpattern(dash)) nolabel yline(0, lp(dash) lcolor(gs10))  graphregion(color(white))  ciopt(recast(rcap) lcolor(black)) mcolor(black) ylabel(0.61 "less than $15k" 0.72 "$15K to 25K" 0.83 "$25K to 35K" 0.94 "$35K to 50K" 1.05 "$50K to 75K" 1.16 "$75K to 100K" 1.27 "$100K to 150K" 1.38 "$150K or more" , nogrid labsize(*0.8)) legend(off) scheme(s1mono)  title("Cooling Balance Point")
graph save "image1.gph", replace
xtreg S3 HP income_factor size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==1, re r
est store m1
xtreg S3 HP income_factor size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==2, re r
est store m2
xtreg S3 HP income_factor size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==3, re r
est store m3
xtreg S3 HP income_factor size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==4, re r
est store m4
xtreg S3 HP income_factor size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==5, re r
est store m5
xtreg S3 HP income_factor size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==6, re r
est store m6
xtreg S3 HP income_factor size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==7, re r
est store m7
xtreg S3 HP income_factor size_factor age_factor ethnic_factor VRESAGE i.V8 if income_factor==8, re r
est store m8
coefplot (m1, keep(HP) mc("230 159 0") lc("230 159 0")    ciopt(recast(ci) lcolor("230 159 0") ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f"))) (m2,keep(HP) mc("230 159 0") lc("230 159 0")  ms(circle) ciopt(recast(ci) lcolor("230 159 0")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f")))  (m3, keep(HP) mc("230 159 0") lc("230 159 0")   ms(circle) ciopt(recast(ci) lcolor("230 159 0")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f"))) (m4,keep(HP) mc("230 159 0") lc("230 159 0")  ms(circle) ciopt(recast(ci) lcolor("230 159 0")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f")))  (m5, keep(HP) mc("230 159 0") lc("230 159 0")   ms(circle) ciopt(recast(ci) lcolor("230 159 0")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f"))) (m6,keep(HP) mc("230 159 0") lc("230 159 0")  ms(circle) ciopt(recast(ci) lcolor("230 159 0")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f"))) (m7, keep(HP) mc("230 159 0") lc("230 159 0")   ms(circle) ciopt(recast(ci) lcolor("230 159 0")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f"))) (m8,keep(HP) mc("230 159 0") lc("230 159 0")  ms(circle) ciopt(recast(ci) lcolor("230 159 0")  ) mlabposition(1) mlabgap(*2) mlabel(string(@b,"%9.3f"))) , xline(0, lc(gs6) lpattern(dash)) nolabel yline(0, lp(dash) lcolor(gs10))  graphregion(color(white))  ciopt(recast(rcap) lcolor(black)) mcolor(black) ylabel(0.61 "less than $15k" 0.72 "$15K to 25K" 0.83 "$25K to 35K" 0.94 "$35K to 50K" 1.05 "$50K to 75K" 1.16 "$75K to 100K" 1.27 "$100K to 150K" 1.38 "$150K or more" , nogrid labsize(*0.8)) legend(off) scheme(s1mono)  title("Cooling Slope")
graph save "image2.gph", replace
****************
graph use "image1.gph"
graph use "image2.gph"
graph combine image1.gph image2.gph, row(1) col(2) 
graph export "combined_image.png", as(png) replace



esttab m1 m2 m3 m4  using stata_1.csv,scalars(r2 r2_w r2_o r2_b) pr2  se  replace nogap  b(%9.4f) star(* 0.10 ** 0.05 *** 0.01)
