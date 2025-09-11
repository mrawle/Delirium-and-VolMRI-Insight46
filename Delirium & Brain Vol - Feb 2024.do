*-----------------------------------------------------ANALYSES-----------------------------------------------------
*USE CURRENT DATA OF ALL 502 INSIGHT PARTICIPANTS
clear
use "PATH//Datasets/FINAL5_analysis2.dta"
drop _merge
merge 1:1 nshdid_ntag1 using "PATH//Datasets/FINAL_fram.dta"
drop _merge
drop if brain_bsi==.
drop if dementia_i46p2==1
drop if mbraindis_i46p1==1 
drop if mjrbraindis_i46p2==1 

gen delirmid = 0
replace delirmid=1 if deliry==2016 & scanage!=70 & scanage!=71 
replace delirmid=1 if deliry==2017 & scanage!=71
replace delirmid=1 if deliry==2018
replace delirmid=1 if deliry==2019
gen delir3 = delir2
replace delir3=0 if delirmid==1

save "PATH//Datasets/FINAL5_analysis3.dta", replace

/*gen delir2=delir
replace delir2=0 if deliry<=1997
recode sex (-9=.)

recode NFL_N4PE_plasma_p1 (-99=.)
gen nfl2 = log(NFL_N4PE_plasma_p1)

gen apoe = ins_apoe_i46p1
recode apoe (0=.) (3=1) (5=1) (6=1) (2=0) (4=0)
label define apoe 0 "Non-carrier" 1 "e4 Carrier"
label variable apoe "APOE (insight 46)"
label values apoe apoe

gen edu = burnham_i46p1
recode edu (2=1) (3=1) (4=2) (5=2) (6=2) (7=2) (8=2)
label define edu 0 "No formal education" 1 "Vocational / O-Level" 2 "A-Level or Higher"
label variable edu "Education Status"
label values edu edu

gen sc = sc1553
recode sc (1=2) (3=2) (4=1) (5=1) (6=1)
label define sc 1 "Manual" 2 "Non-Manual"
label variable sc "Social Class"
label values sc sc

drop if brain_vol1==.
drop if dementia_i46p2==1
drop if mbraindis_i46p1==1 
drop if mjrbraindis_i46p2==1 

rename PP_69 pp_69
replace pp_69 = 1 if pp_69==. & PP_63==1
replace pp_69 = 0 if pp_69==. & PP_63==0

replace disa_69 = 1 if disa_69==. & disa_63==1
replace disa_69 = 0 if disa_69==. & disa_63==0
replace disa_69 = 1 if disa_69==. & disa_53==1
replace disa_69 = 0 if disa_69==. & disa_53==0

rename VR_69 vr_69
rename SS_69 ss_69

replace chrondisease_69 = 0 if chrondisease_69==. & chrondisease_63==0
replace chrondisease_69 = 1 if chrondisease_69==. & chrondisease_63==1
replace chrondisease_69 = 2 if chrondisease_69==. & chrondisease_63==2
replace chrondisease_69 = 3 if chrondisease_69==. & chrondisease_63==3

rename lesiontot_i46p1 wmc_tot
rename vent_bsi2 vent_bsi

gen wmc_10 = wmc_tot /10
egen stdbrain = std(brain_bsi)
egen stdhippo = std(hippoa_bsi)
egen stdvent = std(vent_bsi)
*/

/*use"PATH//Datasets/FINAL5_analysis3.dta",

recode NFL_N4PE_plasma_p1 (-99=.)
gen nfl2 = ln(NFL_N4PE_plasma_p1) / ln(2)

drop if brain_bsi==.
drop if dementia_i46p2==1
drop if mbraindis_i46p1==1 
drop if mjrbraindis_i46p2==1 

keep nshdid_ntag1 sex edu cogchild smokingstat15x spm_tiv_vol1 spm_tiv_vol2 brain_vol1 brain_vol2 brain_bsi hippoa_vol1 hippoa_vol2 hippoa_bsi vent_vol1 vent_vol2 vent_bsi wm_vol_change gm_vol_change spm_wm_vol1 spm_wm_vol2 spm_gm_vol1 spm_gm_vol2 pacc_i46p1 pacc_i46p2 apoe delir2 pp_69 disa_69 chrondisease_69 wmc_tot acetotfin15x gapdatey vr_69 ss_69 nfl2 nfl2 scanage NFL_N4PE_plasma_p1 suvr_composite_wm_pvc_bl status_composite_wm_pvc_bl suvr_composite_wc_pvc_bl status_composite_wc_pvc_bl socialclass wmc_10 stdhippo stdbrain stdvent fhs69_clinic_risk_acc

dtable i.sex cogchild i.edu i.socialclass fhs69_clinic_risk_acc i.pp_69 i.smokingstat15x i.apoe spm_tiv_vol1 wmc_tot i.status_composite_wm_pvc_bl nfl2 nfl2 scanage gapdatey acetotfin15x vr_69 ss_69 pacc_i46p1 pacc_i46p2 brain_bsi hippoa_bsi vent_bsi, by(delir2, nototals) title(Table 1. Sample Characteristics) export("descriptives.docx", replace)

preserve

mi set flong
mi stset, clear
mi xtset, clear 

mi register imputed pacc_i46p2 apoe delir2 wmc_tot acetotfin15x gapdatey vr_69 ss_69 nfl2 suvr_composite_wm_pvc_bl status_composite_wm_pvc_bl suvr_composite_wc_pvc_bl status_composite_wc_pvc_bl wmc_10 fhs69_clinic_risk_acc

mi impute chained (regress) nfl2 acetotfin15x (logit) delir2 apoe status_composite_wm_pvc_bl status_composite_wc_pvc_bl (pmm, knn(5)) fhs69_clinic_risk_acc wmc_10 pacc_i46p2 vr_69 ss_69 = sex edu cogchild smokingstat15x spm_tiv_vol1 brain_vol1 hippoa_vol1 vent_vol1 spm_wm_vol1 spm_gm_vol1 disa_69 chrondisease_69 pacc_i46p1 pp_69 socialclass brain_vol2 brain_bsi hippoa_vol2 hippoa_bsi vent_vol2 vent_bsi wm_vol_change gm_vol_change gapdatey stdhippo stdbrain stdvent, rseed(270186) add(20) augment

save "PATH//Datasets/FINAL5_imputed_analysis_delir.dta", replace
*/


use "PATH//Datasets/FINAL5_analysis3.dta"

recode NFL_N4PE_plasma_p1 (-99=.)
gen nfl2 = ln(NFL_N4PE_plasma_p1) / ln(2)

drop if brain_bsi==.
drop if dementia_i46p2==1
drop if mbraindis_i46p1==1 
drop if mjrbraindis_i46p2==1 

keep nshdid_ntag1 sex edu cogchild smokingstat15x spm_tiv_vol1 spm_tiv_vol2 brain_vol1 brain_vol2 brain_bsi hippoa_vol1 hippoa_vol2 hippoa_bsi vent_vol1 vent_vol2 vent_bsi wm_vol_change gm_vol_change spm_wm_vol1 spm_wm_vol2 spm_gm_vol1 spm_gm_vol2 pacc_i46p1 pacc_i46p2 apoe delir2 pp_69 disa_69 chrondisease_69 wmc_tot acetotfin15x gapdatey vr_69 ss_69 nfl2 scanage NFL_N4PE_plasma_p1 suvr_composite_wm_pvc_bl status_composite_wm_pvc_bl suvr_composite_wc_pvc_bl status_composite_wc_pvc_bl socialclass wmc_10 stdhippo stdbrain stdvent fhs69_clinic_risk_acc delir3

dtable i.sex cogchild i.edu i.socialclass fhs69_clinic_risk_acc i.pp_69 i.smokingstat15x i.apoe spm_tiv_vol1 wmc_tot i.status_composite_wm_pvc_bl nfl2 nfl2 scanage gapdatey acetotfin15x vr_69 ss_69 pacc_i46p1 pacc_i46p2 brain_bsi hippoa_bsi vent_bsi, by(delir2, nototals) title(Table 1. Sample Characteristics) export("descriptives.docx", replace)

preserve

mi set flong
mi stset, clear
mi xtset, clear 

mi register imputed pacc_i46p2 apoe delir2 delir3 wmc_tot acetotfin15x gapdatey vr_69 ss_69 nfl2 suvr_composite_wm_pvc_bl status_composite_wm_pvc_bl suvr_composite_wc_pvc_bl status_composite_wc_pvc_bl wmc_10 fhs69_clinic_risk_acc

mi impute chained (regress) nfl2 acetotfin15x (logit) delir3 apoe status_composite_wm_pvc_bl status_composite_wc_pvc_bl (pmm, knn(5)) fhs69_clinic_risk_acc wmc_10 pacc_i46p2 vr_69 ss_69 = sex edu cogchild smokingstat15x spm_tiv_vol1 brain_vol1 hippoa_vol1 vent_vol1 spm_wm_vol1 spm_gm_vol1 disa_69 chrondisease_69 pacc_i46p1 pp_69 socialclass brain_vol2 brain_bsi hippoa_vol2 hippoa_bsi vent_vol2 vent_bsi wm_vol_change gm_vol_change gapdatey stdhippo stdbrain stdvent, rseed(270186) add(20) augment

save "PATH//Datasets/FINAL5_imputed_analysis_delir3.dta", replace

/*
*IF WANTING TO IMPUTE A SECOND BRAIN SCAN BASED ON OUTCOME OF THE FIRST
mi set flong
mi xtset, clear 

mi register imputed brain_vol2 brain_bsi hippoa_vol2 hippoa_bsi vent_vol2 vent_bsi wm_vol_change gm_vol_change spm_wm_vol2 spm_gm_vol2 pacc_i46p2 apoe delir2 wmc_tot acetotfin15x gapdatey vr_69 ss_69 nfl2 suvr_composite_wm_pvc_bl status_composite_wm_pvc_bl suvr_composite_wc_pvc_bl status_composite_wc_pvc_bl 

mi impute chained (regress) nfl2 acetotfin15x (logit) delir2 apoe status_composite_wm_pvc_bl status_composite_wc_pvc_bl (pmm, knn(5)) brain_bsi hippoa_bsi vent_bsi brain_vol2 hippoa_vol2 vent_vol2 wmc_tot gapdatey wm_vol_change gm_vol_change pacc_i46p2 vr_69 ss_69 = sex edu cogchild smokingstat15x spm_tiv_vol1 brain_vol1 hippoa_vol1 vent_vol1 spm_wm_vol1 spm_gm_vol1 pacc_i46p1 pp_69 disa_69 chrondisease_69 socialclass, rseed(270186) add(25) augment*/

clear
use "PATH//Datasets/FINAL5_analysis3.dta"

recode NFL_N4PE_plasma_p1 (-99=.)
gen nfl2 = ln(NFL_N4PE_plasma_p1) / ln(2)

drop if brain_bsi==.
drop if dementia_i46p2==1
drop if mbraindis_i46p1==1 
drop if mjrbraindis_i46p2==1 


*TABLE ONE COMPLETE CASE
collect clear
collect create completecasetable1

collect _r_b _r_ci _r_p, tag(model[(1)]): regress brain_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(1)]): testparm i.edu#c.gapdatey
collect p_g=r(p), tag(model[(1)]): testparm i.smokingstat15x#c.gapdatey

collect _r_b _r_ci _r_p, tag(model[(2)]): regress hippoa_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(2)]): testparm i.edu#c.gapdatey
collect p_g=r(p), tag(model[(2)]): testparm i.smokingstat15x#c.gapdatey

collect _r_b _r_ci _r_p, tag(model[(3)]): regress vent_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(3)]): testparm i.edu#c.gapdatey
collect p_g=r(p), tag(model[(3)]): testparm i.smokingstat15x#c.gapdatey

collect layout (colname#result result[p_f p_g]) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect style cell border_bottom, border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" × ")
collect style header result[p_f p_g], level(label)
collect label levels result p_f "p-value for educational status"
collect style cell result [p_f], nformat(%5.3f)
collect label levels result p_g "p-value for smoking status"
collect style cell result [p_g], nformat(%5.3f)
collect style cell result[_r_p], nformat(%5.3f)
collect style showbase off
collect notes "BSI: Boundary Shift Integral"
collect style title, font(,bold)
collect title "Table 1: Complete case associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export "PATH//complete_case_analysis_table.docx", replace

*BOOTSTRAP TO CHECK TABLE ONE VALIDITY
bootstrap, reps(2000) seed(270186) bca: regress vent_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estat bootstrap, bca

*TABLE TWO COMPLETE CASE
collect clear
collect create completecasetable2

collect _r_b _r_ci _r_p, tag(model[(1)]): regress brain_bsi delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(1)]): testparm i.edu#c.gapdatey
collect p_g=r(p), tag(model[(1)]): testparm i.smokingstat15x#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(2)]): regress brain_bsi delir2##c.nfl2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(2)]): testparm i.edu#c.gapdatey
collect p_g=r(p), tag(model[(2)]): testparm i.smokingstat15x#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(3)]): regress brain_bsi delir2##c.wmc_10#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(3)]): testparm i.edu#c.gapdatey
collect p_g=r(p), tag(model[(3)]): testparm i.smokingstat15x#c.gapdatey

collect layout (colname#result result[p_f p_g]) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style header result[p_f p_g], level(label)
collect label levels result p_f "p-value for educational status"
collect style cell result [p_f], nformat(%5.3f)
collect label levels result p_g "p-value for smoking status"
collect style cell result [p_g], nformat(%5.3f)
collect style showbase off
collect title "Complete case amyloid, wmhv and nfl2 adjusted associations between total brain volume change and delirium"

collect export "PATH//complete_case_analysis_table2.docx", replace


*-----------------------------------------------------IMPUTED DATA-----------------------------------------------------
*IMPUTE
clear
use "PATH//Datasets/FINAL5_imputed_analysis.dta"

*FINAL MODEL TABLE ONE
collect clear
collect create casetable1

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(1)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(1)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(2)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(2)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(3)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(3)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect layout (colname#result result[p_f p_g]) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect style cell border_bottom, border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" × ")
collect style header result[p_f p_g], level(label)
collect label levels result p_f "p-value for educational status"
collect style cell result [p_f], nformat(%5.3f)
collect label levels result p_g "p-value for smoking status"
collect style cell result [p_g], nformat(%5.3f)
collect style cell result[_r_p], nformat(%5.3f)
collect style showbase off
collect notes "BSI: Boundary Shift Integral"
collect style title, font(,bold)
collect title "Table 1: associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export "PATH//analysis_table.docx", replace

*FINAL MODEL TABLE 2
collect clear
collect create casetable2

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(1)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(1)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress brain_bsi delir2##c.nfl2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(2)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(2)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress brain_bsi delir2##c.wmc_10#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(3)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(3)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect layout (colname#result result[p_f p_g]) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style header result[p_f p_g], level(label)
collect label levels result p_f "p-value for educational status"
collect style cell result [p_f], nformat(%5.3f)
collect label levels result p_g "p-value for smoking status"
collect style cell result [p_g], nformat(%5.3f)
collect style showbase off
collect title "amyloid, wmhv and nfl2 adjusted associations between total brain volume change and delirium"

collect export "PATH//analysis_table2.docx", replace

*-----------------------------------------------CROSS SECTIONAL OUTCOMES--------------------------------------------------
*WAVE ONE VOLUMES CROSS SECTIONAL
collect clear
collect create casetable1

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_vol1 delir3 sex cogchild i.edu socialclass fhs69_clinic_risk_acc pp_69 i.smokingstat15x apoe spm_tiv_vol1 wmc_10 status_composite_wm_pvc_bl nfl2 scanage
collect p_f=r(p), tag(model[(1)]): mi test 1.edu 2.edu
collect p_g=r(p), tag(model[(1)]): mi test 1.smokingstat15x 2.smokingstat15x

collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa_vol1 delir3 sex cogchild i.edu socialclass fhs69_clinic_risk_acc pp_69 i.smokingstat15x apoe spm_tiv_vol1 wmc_10 status_composite_wm_pvc_bl nfl2 scanage
collect p_f=r(p), tag(model[(2)]): mi test 1.edu 2.edu
collect p_g=r(p), tag(model[(2)]): mi test 1.smokingstat15x 2.smokingstat15x

collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_vol1 delir3 sex cogchild i.edu socialclass fhs69_clinic_risk_acc pp_69 i.smokingstat15x apoe spm_tiv_vol1 wmc_10 status_composite_wm_pvc_bl nfl2 scanage
collect p_f=r(p), tag(model[(3)]): mi test 1.edu 2.edu
collect p_g=r(p), tag(model[(3)]): mi test 1.smokingstat15x 2.smokingstat15x

collect layout (colname#result result[p_f p_g]) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect style cell border_bottom, border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" × ")
collect style header result[p_f p_g], level(label)
collect label levels result p_f "p-value for educational status"
collect style cell result [p_f], nformat(%5.3f)
collect label levels result p_g "p-value for smoking status"
collect style cell result [p_g], nformat(%5.3f)
collect style cell result[_r_p], nformat(%5.3f)
collect style showbase off
collect notes "BSI: Boundary Shift Integral"
collect style title, font(,bold)
collect title "Table 1: delir 3 associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export "PATH//CSanalysis_table_wave1_delir3.docx", replace

*WAVE TWO VOLUMES CROSS SECTIONAL
collect clear
collect create casetable1

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_vol2 delir2 sex cogchild i.edu socialclass fhs69_clinic_risk_acc pp_69 i.smokingstat15x apoe spm_tiv_vol1 wmc_10 status_composite_wm_pvc_bl nfl2 scanage
collect p_f=r(p), tag(model[(1)]): mi test 1.edu 2.edu
collect p_g=r(p), tag(model[(1)]): mi test 1.smokingstat15x 2.smokingstat15x

collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa_vol2 delir2 sex cogchild i.edu socialclass fhs69_clinic_risk_acc pp_69 i.smokingstat15x apoe spm_tiv_vol1 wmc_10 status_composite_wm_pvc_bl nfl2 scanage
collect p_f=r(p), tag(model[(2)]): mi test 1.edu 2.edu
collect p_g=r(p), tag(model[(2)]): mi test 1.smokingstat15x 2.smokingstat15x

collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_vol2 delir2 sex cogchild i.edu socialclass fhs69_clinic_risk_acc pp_69 i.smokingstat15x apoe spm_tiv_vol1 wmc_10 status_composite_wm_pvc_bl nfl2 scanage
collect p_f=r(p), tag(model[(3)]): mi test 1.edu 2.edu
collect p_g=r(p), tag(model[(3)]): mi test 1.smokingstat15x 2.smokingstat15x

collect layout (colname#result result[p_f p_g]) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect style cell border_bottom, border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" × ")
collect style header result[p_f p_g], level(label)
collect label levels result p_f "p-value for educational status"
collect style cell result [p_f], nformat(%5.3f)
collect label levels result p_g "p-value for smoking status"
collect style cell result [p_g], nformat(%5.3f)
collect style cell result[_r_p], nformat(%5.3f)
collect style showbase off
collect notes "BSI: Boundary Shift Integral"
collect style title, font(,bold)
collect title "Table 1: Complete case associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export "PATH//CSanalysis_table_wave2.docx", replace

*COGNITIVE OUTCOMES
collect clear
collect create casetable1

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress acetotfin15x delir3 sex cogchild i.edu socialclass apoe fhs69_clinic_risk_acc
collect p_e=r(p), tag(model[(1)]): mi test 1.edu 2.edu
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress vr_69 delir3 sex cogchild i.edu socialclass apoe fhs69_clinic_risk_acc
collect p_e=r(p), tag(model[(2)]): mi test 1.edu 2.edu
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress ss_69 delir3 sex cogchild i.edu socialclass apoe fhs69_clinic_risk_acc
collect p_e=r(p), tag(model[(3)]): mi test 1.edu 2.edu
collect _r_b _r_ci _r_p, tag(model[(4)]): mi estimate: regress pacc_i46p1 delir3 sex cogchild i.edu socialclass apoe fhs69_clinic_risk_acc
collect p_e=r(p), tag(model[(4)]): mi test 1.edu 2.edu
collect _r_b _r_ci _r_p, tag(model[(5)]): mi estimate: regress pacc_i46p2 delir2 sex cogchild i.edu socialclass apoe fhs69_clinic_risk_acc
collect p_e=r(p), tag(model[(5)]): mi test 1.edu 2.edu

collect layout (colname#result result[p_e]) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect style cell border_bottom, border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" × ")
collect style header result[p_e p_f p_g], level(label)
collect label levels result p_e  "p-value for educational status"
collect style cell result [p_e], nformat(%5.3f)
collect style showbase off
collect style title, font(,bold)
collect title "Cognitive outcomes, ace, vr, ss, pacc1, pacc2"

collect export "PATH//CSanalysis_table_cog_delir3.docx", replace

*----------------------------------------------SENSITIVITY ANALYSES-------------------------------------------------------

*SEX ADJUSTED MODEL TABLE ONE
collect clear
collect create casetable1

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa_bsi delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_bsi delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons

collect layout (colname#result) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect style cell border_bottom, border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" × ")
collect style cell result[_r_p], nformat(%5.3f)
collect style showbase off
collect notes "BSI: Boundary Shift Integral"
collect style title, font(,bold)
collect title "Table 1: Complete case associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export "PATH//limitedmodel_table.docx", replace

*SEX ADJUSTED TABLE 2
collect clear
collect create casetable2

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress brain_bsi delir2##c.nfl2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress brain_bsi delir2##c.wmc_10#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons

collect layout (colname#result) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style showbase off
collect title "Complete case amyloid, wmhv and nfl2 adjusted associations between total brain volume change and delirium"

collect export "PATH//limitedmodel_table2.docx", replace

*AMYLOID REF CHANGE TABLE 1
collect clear
collect create casetable1

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wc_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(1)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(1)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wc_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(2)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(2)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wc_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(3)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(3)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect layout (colname#result result[p_f p_g]) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect style cell border_bottom, border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" × ")
collect style header result[p_f p_g], level(label)
collect label levels result p_f "p-value for educational status"
collect style cell result [p_f], nformat(%5.3f)
collect label levels result p_g "p-value for smoking status"
collect style cell result [p_g], nformat(%5.3f)
collect style cell result[_r_p], nformat(%5.3f)
collect style showbase off
collect notes "BSI: Boundary Shift Integral"
collect style title, font(,bold)
collect title "Table 1: Complete case associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export "PATH//analysiscerebellum_table.docx", replace

*AMYLOID REF CHANGE TABLE 2
collect clear
collect create casetable2

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi delir2##status_composite_wc_pvc_bl#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(1)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(1)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress brain_bsi delir2##c.nfl2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wc_pvc_bl#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(2)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(2)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress brain_bsi delir2##c.wmc_10#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey status_composite_wc_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(3)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(3)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect layout (colname#result result[p_f p_g]) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style header result[p_f p_g], level(label)
collect label levels result p_f "p-value for educational status"
collect style cell result [p_f], nformat(%5.3f)
collect label levels result p_g "p-value for smoking status"
collect style cell result [p_g], nformat(%5.3f)
collect style showbase off
collect title "Complete case amyloid, wmhv and nfl2 adjusted associations between total brain volume change and delirium"

collect export "PATH//analysiscerebellum_table2.docx", replace


*AMYLOID SUVR CHANGE TABLE 1
collect clear
collect create casetable1

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey c.suvr_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(1)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(1)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey c.suvr_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(2)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(2)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey c.suvr_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(3)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(3)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect layout (colname#result result[p_f p_g]) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect style cell border_bottom, border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" × ")
collect style header result[p_f p_g], level(label)
collect label levels result p_f "p-value for educational status"
collect style cell result [p_f], nformat(%5.3f)
collect label levels result p_g "p-value for smoking status"
collect style cell result [p_g], nformat(%5.3f)
collect style cell result[_r_p], nformat(%5.3f)
collect style showbase off
collect notes "BSI: Boundary Shift Integral"
collect style title, font(,bold)
collect title "Table 1: Complete case associations between total brain, hippocampal and ventricular volume change (BSI) and delirium suvr adjusted"

collect export "PATH//analysissuvr_table.docx", replace

*AMYLOID SUVR CHANGE TABLE 2
collect clear
collect create casetable2

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi delir2##c.suvr_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(1)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(1)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress brain_bsi delir2##c.nfl2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey c.suvr_composite_wm_pvc_bl#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(2)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(2)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress brain_bsi delir2##c.wmc_10#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.suvr_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(3)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(3)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect layout (colname#result result[p_f p_g]) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style header result[p_f p_g], level(label)
collect label levels result p_f "p-value for educational status"
collect style cell result [p_f], nformat(%5.3f)
collect label levels result p_g "p-value for smoking status"
collect style cell result [p_g], nformat(%5.3f)
collect style showbase off
collect title "Complete case amyloid, wmhv and nfl2 adjusted associations between total brain volume change and delirium suvr adjusted"

collect export "PATH//analysissuvr_table2.docx", replace

*DISPROPORTIONATE BSI MODEL TABLE ONE
collect clear
collect create sensewholebrainbsi1

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress hippoa_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wc_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.brain_bsi#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(1)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(1)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect layout (colname#result result[p_f p_g]) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style header result[p_f p_g], level(label)
collect label levels result p_f "p-value for educational status"
collect style cell result [p_f], nformat(%5.3f)
collect label levels result p_g "p-value for smoking status"
collect style cell result [p_g], nformat(%5.3f)
collect style showbase off
collect title "Associations between hippocampal and ventricular volume change (BSI) and delirium, adjusted for baseline rate of whole-brain change"

collect export "PATH//hippobsi_table.docx", replace

*-----------------------------------------------------GENERATE GRAPHS-----------------------------------------------------
*FOREST PLOT (FIGURE 1)
use "PATH//Datasets/FINAL5_imputed_analysis.dta", clear
set scheme mrc
estimates drop _all
graph drop _all
mi estimate: qui regress brain_bsi delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estimates store Aa
mi estimate: qui regress brain_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estimates store Bb
mi estimate: qui regress hippoa_bsi delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estimates store Cc
mi estimate: qui regress hippoa_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estimates store Dd
mi estimate: qui regress vent_bsi delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estimates store Ee
mi estimate: qui regress vent_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estimates store Ff
estout Aa Bb Cc Dd Ee Ff
coefplot Aa Bb, keep(1.delir2#c.gapdatey c.wmc_10#c.gapdatey 1.status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey) xline(0) nolabels name(graph1) title("Whole Brain") coeflabels(1.delir2#c.gapdatey = "Delirium" c.wmc_10#c.gapdatey = "WMHV" 1.status_composite_wm_pvc_bl#c.gapdatey = "Aβ Positivity" c.nfl2#c.gapdatey = "NfL") ciopts(recast(rcap)) legend(order(2 "Model one" 4 "Model two"))
coefplot Cc Dd, keep(1.delir2#c.gapdatey c.wmc_10#c.gapdatey 1.status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey) xline(0) nolabels name(graph2) title("Hippocampus") coeflabels(1.delir2#c.gapdatey = "Delirium" c.wmc_10#c.gapdatey = "WMHV" 1.status_composite_wm_pvc_bl#c.gapdatey = "Aβ Positivity" c.nfl2#c.gapdatey = "NfL") ciopts(recast(rcap)) legend(order(2 "Model one" 4 "Model two"))
coefplot Ee Ff, keep(1.delir2#c.gapdatey c.wmc_10#c.gapdatey 1.status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey) xline(0) nolabels name(graph3) title("Ventricles") coeflabels(1.delir2#c.gapdatey = "Delirium" c.wmc_10#c.gapdatey = "WMHV" 1.status_composite_wm_pvc_bl#c.gapdatey = "Aβ Positivity" c.nfl2#c.gapdatey = "NfL") ciopts(recast(rcap)) legend(order(2 "Model one" 4 "Model two"))
graph combine graph1 graph2 graph3

graph export "PATH//forest_plot22.pdf", replace



*FOREST PLOT (FIGURE 1v2)
use "PATH//Datasets/FINAL5_imputed_analysis.dta", clear
estimates drop _all
graph drop _all

*graph query, schemes
set scheme mrc

mi estimate: qui regress stdbrain delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estimates store Aa
mi estimate: qui regress stdbrain delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estimates store Bb
mi estimate: qui regress stdhippo delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estimates store Cc
mi estimate: qui regress stdhippo delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estimates store Dd
mi estimate: qui regress stdvent delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estimates store Ee
mi estimate: qui regress stdvent delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estimates store Ff
estout Aa Bb Cc Dd Ee Ff
coefplot Aa Bb Cc Dd Ee Ff, keep(1.delir2#c.gapdatey c.wmc_10#c.gapdatey 1.status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey) vertical nolabels coeflabels(1.delir2#c.gapdatey = "Delirium" c.wmc_10#c.gapdatey = "WMHV" 1.status_composite_wm_pvc_bl#c.gapdatey = "Aβ Positivity" c.nfl2#c.gapdatey = "NfL") ciopts(recast(rcap)) yline(0, lp(dash)) legend(size(vsmall) symx(small) pos(6) order(2 "Whole Brain (Model one)" 6 "Hippocampal (Model one)" 10 "Ventricular (Model one)" 4 "Whole Brain (Model two)" 8 "Hippocampal (Model two)" 12 "Ventricular (Model two)") cols(3) rows(2)) ytitle("Standardised BSI")

graph export "PATH//forest_plot_v22.pdf", replace




use "PATH//Datasets/FINAL5_analysis3.dta", clear
foreach y in brain_bsi hippoa_bsi vent_bsi {
	 qui regress `y' delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
	summarize gapdatey
	local meantime = r(mean)
	margins delir2, at(gapdatey=(`meantime'))
	marginsplot, title("`y' by Delirium Status") subtitle("At mean follow-up time") ytitle("Predicted `y'") xlabel(0 "No Delirium" 1 "Delirium") name(`y'_plot, replace)
}

graph combine brain_bsi_plot hippoa_bsi_plot vent_bsi_plot, title("Predicted brain metrics by delirium status") note("Fully adjusted")

graph export "PATH//margins_plot_v2.pdf", replace

use "PATH//Datasets/FINAL5_analysis3.dta", clear

/*use margins_brain_bsi, clear
rename brain_bsi_margin margin
gen outcome = "Total Brain"
append using margins_hippoa_bsi
replace margin = hippoa_bsi_margin if hippoa_bsi_margin !=.
replace outcome = "Hippocampus" if outcome==""
append using margins_vent_bsi 
replace margin = vent_bsi_margin if vwnt_bsi_margin !=.
replace outcome = "Ventricle" if outcome==""

graph hbar margin, over(delir2) over(outcome) title("Predicted brain metrics by delirium status") subtitle("Evaluated at mean follow up time") ytitle("Predicted BSI") note("Fully adjusted")
graph export "PATH//margins_plot.pdf"
*/

*--------------------------------------------------POST HOC ANALYSES--------------------------------------------------
*WM GM CHANGE
use "PATH//Datasets/FINAL5_imputed_analysis.dta", clear
/*gen nfl2 = nfl / ln(2)
save "PATH//Datasets/FINAL5_imputed_analysis.dta", replace
*/
collect clear
collect create wmgmtable2

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress wm_vol_change delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(1)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(1)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress gm_vol_change delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_f=r(p), tag(model[(2)]): mi test 1.edu#c.gapdatey 2.edu#c.gapdatey
collect p_g=r(p), tag(model[(2)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect layout (colname#result result[p_f p_g]) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect style cell border_bottom, border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" × ")
collect style header result[p_f p_g], level(label)
collect label levels result p_f "p-value for educational status"
collect style cell result [p_f], nformat(%5.3f)
collect label levels result p_g "p-value for smoking status"
collect style cell result [p_g], nformat(%5.3f)
collect style cell result[_r_p], nformat(%5.3f)
collect style showbase off
collect notes "BSI: Boundary Shift Integral"
collect style title, font(,bold)
collect title "Table 1: White and grey matter change versus delirium"

collect export "PATH//whitegreymatterchange_table.docx", replace

collect clear
collect create wmgmtable3

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress wm_vol_change delir2#c.gapdatey sex#c.gapdatey  c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress gm_vol_change delir2#c.gapdatey sex#c.gapdatey  c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons

collect layout (colname#result) (model)
collect style cell, nformat(%5.3f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.3f)
collect style cell border_block, border(right, pattern(nil))
collect style cell border_bottom, border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" × ")
collect style cell result[_r_p], nformat(%5.3f)
collect style showbase off
collect notes "BSI: Boundary Shift Integral"
collect style title, font(,bold)
collect title "Table 1: White and grey matter change versus delirium"

collect export "PATH//whitegreymatterchange_table2.docx", replace



***FINAL GRAPH (JN SUGGESTION)

*FOREST PLOT (FIGURE 1)
use "PATH//Datasets/FINAL5_imputed_analysis.dta", clear
egen stdgm = std(gm_vol_change)
egen stdwm = std(wm_vol_change)
set scheme mrc
estimates drop _all
graph drop _all
mi estimate: qui regress brain_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
_estimates hold miresults, copy
mimrgns, at(delir2 = (0) gapdatey = (0.01 1 2 3)) post
estimates store brain_nodelir
_estimates unhold miresults
mimrgns, at(delir2 = (1) gapdatey = (0.01 1 2 3)) post
estimates store brain_delir
mi estimate: qui regress hippoa_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
_estimates hold miresults, copy
mimrgns, at(delir2 = (0) gapdatey = (0.01 1 2 3)) post
estimates store hippoa_nodelir
_estimates unhold miresults
mimrgns, at(delir2 = (1) gapdatey = (0.01 1 2 3)) post
estimates store hippoa_delir
mi estimate: qui regress vent_bsi delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
_estimates hold miresults, copy
mimrgns, at(delir2 = (0) gapdatey = (0.01 1 2 3)) post
estimates store vent_nodelir
_estimates unhold miresults
mimrgns, at(delir2 = (1) gapdatey = (0.01 1 2 3)) post
estimates store vent_delir
mi estimate: regress wm_vol_change delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
_estimates hold miresults, copy
mimrgns, at(delir2 = (0) gapdatey = (0.01 1 2 3)) post
estimates store wm_nodelir
_estimates unhold miresults
mimrgns, at(delir2 = (1) gapdatey = (0.01 1 2 3)) post
estimates store wm_delir
mi estimate: regress gm_vol_change delir2#c.gapdatey sex#c.gapdatey c.cogchild#c.gapdatey i.edu#c.gapdatey socialclass#c.gapdatey c.fhs69_clinic_risk_acc#c.gapdatey pp_69#c.gapdatey i.smokingstat15x#c.gapdatey apoe#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.wmc_10#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey c.nfl2#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
_estimates hold miresults, copy
mimrgns, at(delir2 = (0) gapdatey = (0.01 1 2 3)) post
estimates store gm_nodelir
_estimates unhold miresults
mimrgns, at(delir2 = (1) gapdatey = (0.01 1 2 3)) post
estimates store gm_delir

graph drop _all
set scheme mrc

coefplot (brain_nodelir, offset(0) lp(shortdash) )(brain_delir, offset(0.05) msymbol(dh)), recast(connected) vertical nolabels coeflabels(1._at = "0" 2._at = "1" 3._at = "2" 4._at = "3") xtitle("Years") ciopts(recast(rcap)) title("Whole brain", pos(12)) drop(_cons) yline(0, lp(dash)) ytitle("Mean volume change (mls)") legend(size(medium) symx(medium) pos(3) order(2 "No delirium" 4 "Delirium") cols(1) rows(2)) name(graph1)

coefplot (hippoa_nodelir, offset(0) lp(shortdash) )(hippoa_delir, offset(0.05) msymbol(dh)), recast(connected) vertical nolabels coeflabels(1._at = "0" 2._at = "1" 3._at = "2" 4._at = "3") xtitle("Years") ciopts(recast(rcap)) title("Hippocampus", pos(12)) drop(_cons) yline(0, lp(dash)) ytitle("Mean volume change (mls)") legend(size(medium) symx(medium) pos(3) order(2 "No delirium" 4 "Delirium") cols(1) rows(2)) name(graph2)

coefplot (vent_nodelir, offset(0) lp(shortdash) )(vent_delir, offset(0.05) msymbol(dh)), recast(connected) vertical nolabels coeflabels(1._at = "0" 2._at = "1" 3._at = "2" 4._at = "3") xtitle("Years") ciopts(recast(rcap)) title("Ventricles", pos(12)) drop(_cons) yline(0, lp(dash)) ytitle("Mean volume change (mls)") legend(size(medium) symx(medium) pos(3) order(2 "No delirium" 4 "Delirium") cols(1) rows(2)) name(graph3)

coefplot (gm_nodelir, offset(0) lp(shortdash) )(gm_delir, offset(0.05) msymbol(dh)), recast(connected) vertical nolabels coeflabels(1._at = "0" 2._at = "1" 3._at = "2" 4._at = "3") xtitle("Years") ciopts(recast(rcap)) title("Grey matter", pos(12)) drop(_cons) yline(0, lp(dash)) ytitle("Mean volume change (mls)") legend(size(medium) symx(medium) pos(3) order(2 "No delirium" 4 "Delirium") cols(1) rows(2)) name(graph4)

coefplot (wm_nodelir, offset(0) lp(shortdash) )(wm_delir, offset(0.05) msymbol(dh)), recast(connected) vertical nolabels coeflabels(1._at = "0" 2._at = "1" 3._at = "2" 4._at = "3") xtitle("Years") ciopts(recast(rcap)) title("White matter", pos(12)) drop(_cons) yline(0, lp(dash)) ytitle("Mean volume change (mls)") legend(size(medium) symx(medium) pos(3) order(2 "No delirium" 4 "Delirium") cols(1) rows(2)) name(graph5)

grc1leg graph1 graph2 graph3 graph4 graph5, legendfrom(graph1)
