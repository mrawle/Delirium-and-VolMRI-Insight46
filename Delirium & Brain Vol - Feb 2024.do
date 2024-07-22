*-----------------------------------------------------LOAD DATASET-----------------------------------------------------
use "S:\LHA_MR1021\Amyloid, Volumetric & DTI x ACBS\Datasets\Working\smallunimputed0908.dta", clear
merge 1:1 nshdid_ntag1 using "S:\LHA_MR1021\Amyloid, Volumetric & DTI x ACBS\Datasets\Unedited\memorytest_vars.dta"
drop _merge
merge 1:1 nshdid_ntag1 using "S:\LHA_MR1021\Amyloid, Volumetric & DTI x ACBS\Datasets\Unedited\sc1553.dta"
drop _merge
merge 1:1 nshdid_ntag1 using "S:\LHA_MR1021\Amyloid, Volumetric & DTI x ACBS\Datasets\whitematter.dta"
drop _merge

*-----------------------------------------------------DATA CLEANING-----------------------------------------------------
*RECODE MISSING VARIABLES AND GENERATE APPROPRIATE STRATA FOR ANALYSIS
recode lesiontot_i46p1 (-9=.) (-99=.)
recode mmse_total_i46p1 (-99=.) (-9=.)
recode mmse_zscore_i46p1 (-99=.) (-9=.)
recode bamosuseable_i46p1 (-99=.) (-9=.)
recode es_i46p2 (-99=.) (-9=.)
recode ez_i46p2 (-99=.) (-9=.)
recode pacc_i46p1 (-99=.) (-9=.)
recode pacc_i46p2 (-99=.) (-9=.)

gen wmc_tot = lesiontot_i46p1
replace wmc_tot=. if bamosuseable_i46p1==0

gen i46_edu = .
replace i46_edu=0 if qual_i46p1=="no qualification" 
replace i46_edu=1 if qual_i46p1=="below ordinary secondary qualifications" | qual_i46p1=="ordinary level qualifications"
replace i46_edu=2 if qual_i46p1=="advanced level qualifications" | qual_i46p1=="higher education"
label define i46_edu 0 "None" 1 "Vocational / O-Level" 2 "A-Level / Higher"
label variable i46_edu "Education (Insight46)"
label values i46_edu i46_edu

gen socialclass = sc1553
recode socialclass (1=30) (2=30) (3=20) (4=20) (5=10) (6=10)
recode socialclass (30=3) (20=2) (10=1)
label define socialclass 1 "V / IV" 2 "IIIM / III" 3 "II / I"
label variable socialclass "Social Class @ 53"
label values socialclass socialclass

*CREATE VARIABLE TO DEFINE BRAIN CHANGE
gen brainchange = (brain_bsi2 / brain_vol1) * 100
gen ventchange = (vent_bsi2 / vent_vol1) * 100
gen hippoachange = (hippoa_bsi2 / hippoa_vol1) * 100

*CATEGORISE BRAIN DISORDERS TO CHECK CONFOUNDING OF SAMPLE
gen mci_none=0
replace mci_none=1 if mci_i46p2!=1
gen dementia_none=0
replace dementia_none=1 if dementia_i46p2!=1
gen mcidementia_none=0
replace mcidementia_none=1 if dementia_i46p2!=1 &  mci_i46p2!=1
gen mjrbrain_none=0
replace mjrbrain_none=1 if mbraindis_i46p1!=1
gen mjrbrain2wave_none=0
replace mjrbrain2wave_none=1 if mbraindis_i46p1!=1 & mjrbraindis_i46p2!=1
gen temporal_wc_true=0
replace temporal_wc_true=1 if suvr_temporal_wc_change2!=.
gen temporal_wm_true=0
replace temporal_wm_true=1 if suvr_temporal_wm_change2!=.
gen parietal_wc_true=0
replace parietal_wc_true=1 if suvr_parietal_wc_change2!=.
gen parietal_wm_true=0
replace parietal_wm_true=1 if suvr_parietal_wm_change2!=.
gen composite_wc_true=0
replace composite_wc_true=1 if suvr_composite_wc_change2!=.
gen composite_wm_true=0
replace composite_wm_true=1 if suvr_composite_wm_change2!=.

*RECODE APOE TO MIMIC PRIOR INSIGHT 46 PAPERS
gen APOE2 = APOEnew
recode APOE2 (1=0) (2=1)
gen apoe3 = APOEnew
recode apoe3 (2=1)

*RECODE PP FOR DTABLE
gen pp_69 = PP_69

*CREATE MISSING TABLE TO CHECK DATASET
drop if brain_bsi2==.
misstable sum chrondisease_69 disa_69 cogchild attain_26 delir2 sex hippoa_bsi2 brain_bsi2 pp_69 gapdatey APOEnew wmc_tot mci_i46p1 mci_i46p2 smokingstat15x suvr_composite_wc_change2 suvr_parietal_wc_change2 suvr_temporal_wc_change2 suvr_composite_wm_change2 suvr_temporal_wm_change2 suvr_parietal_wm_change2 suvr_composite_wm_bl suvr_parietal_wm_bl suvr_temporal_wm_bl suvr_parietal_wc_bl suvr_composite_wc_bl suvr_temporal_wc_bl, generate(M)

save "use "S:\LHA_MR1021\Amyloid, Volumetric & DTI x ACBS\2024\working set for upload.dta", clear

*-----------------------------------------------------ANALYSES-----------------------------------------------------
*-----------------------------------------------------UNIMPUTED DATA-----------------------------------------------------
*DESCRIPTIVE TABLE
use "S:\LHA_MR1021\Amyloid, Volumetric & DTI x ACBS\2024\working set for upload.dta", clear
drop if mjrbrain_none!=1

dtable cogchild i.i46_edu i.socialclass i.chrondisease_69 i.disa_69 i.pp_69 i.sex i.smokingstat15x i.apoe3 brain_vol1 brain_vol2 pacc_i46p1 pacc_i46p2 wmc_tot i.status_composite_wm_pvc_bl scanage, by(delir2, nototals testnotes test) title(Table 1. Sample Characteristics) export(JUL dtable.docx, replace)
gen delirmiss=0
replace delirmiss=1 if delir2==.
dtable cogchild i.i46_edu i.socialclass i.chrondisease_69 i.disa_69 i.pp_69 i.sex i.smokingstat15x i.apoe3 brain_vol1 brain_vol2 pacc_i46p1 pacc_i46p2 wmc_tot i.status_composite_wm_pvc_bl scanage, by(delirmiss, nototals testnotes test) title(Table 1b. Missing Sample Characteristics) export(JUL dtable miss.docx, replace)

*TABLE ONE COMPLETE CASE
collect clear
collect create completecasetable1

collect _r_b _r_ci _r_p, tag(model[(1)]): regress brain_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(1)]): testparm i.chrondisease_69#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(2)]): regress hippoa2_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(2)]): testparm i.chrondisease_69#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(3)]): regress vent_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(3)]): testparm i.chrondisease_69#c.gapdatey

collect layout (colname#result result[p_e]) (model)
collect style cell, nformat(%5.2f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.2f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style header result[p_e], level(label)
collect label levels result p_e "p-value for chronic disease count"
collect style cell result [p_e], nformat(%5.2f)
collect style showbase off
collect title "Complete case associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export JULcompletecasetable1.docx, replace

*TABLE TWO COMPLETE CASE
collect clear
collect create completecasetable2

collect _r_b _r_ci _r_p, tag(model[(1)]): regress brain_bsi2 delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(1)]): testparm i.chrondisease_69#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(2)]): regress hippoa2_bsi2 delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(2)]): testparm i.chrondisease_69#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(3)]): regress vent_bsi2 delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(3)]): testparm i.chrondisease_69#c.gapdatey

collect layout (colname#result result[p_e]) (model)
collect style cell, nformat(%5.2f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.2f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style header result[p_e], level(label)
collect label levels result p_e "p-value for chronic disease count"
collect style cell result [p_e], nformat(%5.2f)
collect style showbase off
collect title "Complete case amyloid adjusted associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export JULcompletecasetable2.docx, replace


*-----------------------------------------------------IMPUTED DATA-----------------------------------------------------
*IMPUTE
use "S:\LHA_MR1021\Amyloid, Volumetric & DTI x ACBS\2024\working set for upload.dta", clear
drop if mjrbrain_none!=1
mi set flong
mi register imputed chrondisease_69 disa_69 delir2 pp_69 apoe3 wmc_tot
mi impute chained (logit) disa_69 delir2 pp_69 apoe3 (ologit) chrondisease_69 (pmm, knn(5)) wmc_tot = brain_bsi2 hippoa2_bsi2 vent_bsi2 spm_tiv_vol1 cogchild smokingstat15x i46_edu gapdatey sex socialclass scanage, by(status_composite_wm_pvc_bl) rseed(270186) add(10) augment

*FINAL MODEL TABLE ONE
collect clear
collect create finalmodeltable1

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(1)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa2_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(2)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(3)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey

collect layout (colname#result result[p_e]) (model)
collect style cell, nformat(%5.2f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.2f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style header result[p_e], level(label)
collect label levels result p_e "p-value for chronic disease count"
collect style cell result [p_e], nformat(%5.2f)
collect style showbase off
collect title "Associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export finalmodeltable1.docx, replace

*FINAL MODEL TABLE 2
collect clear
collect create finalmodeltable2

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi2 delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(1)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa2_bsi2 delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(2)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_bsi2 delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(3)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey

collect layout (colname#result result[p_e]) (model)
collect style cell, nformat(%5.2f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.2f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style header result[p_e], level(label)
collect label levels result p_e "p-value for chronic disease count"
collect style cell result [p_e], nformat(%5.2f)
collect style showbase off
collect title "Associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export finalmodeltable2.docx, replace

*----------------------------------------------SENSITIVITY ANALYSES-------------------------------------------------------

*SEX ADJUSTED MODEL TABLE ONE
collect clear
collect create sexadjustedtable1

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa2_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons

collect layout (colname#result) (model)
collect style cell, nformat(%5.2f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.2f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style showbase off
collect title "Sex-adjusted associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export sexadjustedtable1.docx, replace

*SEX ADJUSTED TABLE 2
collect clear
collect create sexadjustedtable2

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi2 delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa2_bsi2 delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_bsi2 delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons

collect layout (colname#result) (model)
collect style cell, nformat(%5.2f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.2f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style showbase off
collect title "Sex-adjusted associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export sexadjustedtable2.docx, replace

*A PRIORI TABLE 1
collect clear
collect create aprioritable1

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey socialclass#c.gapdatey i46_edu#c.gapdatey c.cogchild#c.gapdatey smokingstat15x#c.gapdatey disa_69#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(1)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect p_f=r(p), tag(model[(1)]): mi test 1.socialclass#c.gapdatey 2.socialclass#c.gapdatey
collect p_g=r(p), tag(model[(1)]): mi test 1.i46_edu#c.gapdatey 2.i46_edu#c.gapdatey
collect p_h=r(p), tag(model[(1)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa2_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey socialclass#c.gapdatey i46_edu#c.gapdatey c.cogchild#c.gapdatey smokingstat15x#c.gapdatey disa_69#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(2)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect p_f=r(p), tag(model[(2)]): mi test 1.socialclass#c.gapdatey 2.socialclass#c.gapdatey
collect p_g=r(p), tag(model[(2)]): mi test 1.i46_edu#c.gapdatey 2.i46_edu#c.gapdatey
collect p_h=r(p), tag(model[(2)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey socialclass#c.gapdatey i46_edu#c.gapdatey c.cogchild#c.gapdatey smokingstat15x#c.gapdatey disa_69#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(3)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect p_f=r(p), tag(model[(3)]): mi test 1.socialclass#c.gapdatey 2.socialclass#c.gapdatey
collect p_g=r(p), tag(model[(3)]): mi test 1.i46_edu#c.gapdatey 2.i46_edu#c.gapdatey
collect p_h=r(p), tag(model[(3)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect layout (colname#result result[p_e p_f p_g p_h]) (model)
collect style cell, nformat(%5.2f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.2f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style header result[p_e p_f p_g p_h], level(label)
collect label levels result p_e "p-value for chronic disease count" p_f "p-value for social class" p_g "p-value for educational attainment" p_h "p-value for smoking status"
collect style cell result [p_e p_f p_g p_h], nformat(%5.2f)
collect style showbase off
collect title "A priori model associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export aprioritable1.docx, replace

*A PRIORI TABLE 2
collect clear
collect create aprioritable2

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi2 delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey socialclass#c.gapdatey i46_edu#c.gapdatey c.cogchild#c.gapdatey smokingstat15x#c.gapdatey disa_69#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(1)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect p_f=r(p), tag(model[(1)]): mi test 1.socialclass#c.gapdatey 2.socialclass#c.gapdatey
collect p_g=r(p), tag(model[(1)]): mi test 1.i46_edu#c.gapdatey 2.i46_edu#c.gapdatey
collect p_h=r(p), tag(model[(1)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa2_bsi2 delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey socialclass#c.gapdatey i46_edu#c.gapdatey c.cogchild#c.gapdatey smokingstat15x#c.gapdatey disa_69#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(2)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect p_f=r(p), tag(model[(2)]): mi test 1.socialclass#c.gapdatey 2.socialclass#c.gapdatey
collect p_g=r(p), tag(model[(2)]): mi test 1.i46_edu#c.gapdatey 2.i46_edu#c.gapdatey
collect p_h=r(p), tag(model[(2)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_bsi2 delir2##status_composite_wm_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey socialclass#c.gapdatey i46_edu#c.gapdatey c.cogchild#c.gapdatey smokingstat15x#c.gapdatey disa_69#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(3)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect p_f=r(p), tag(model[(3)]): mi test 1.socialclass#c.gapdatey 2.socialclass#c.gapdatey
collect p_g=r(p), tag(model[(3)]): mi test 1.i46_edu#c.gapdatey 2.i46_edu#c.gapdatey
collect p_h=r(p), tag(model[(3)]): mi test 1.smokingstat15x#c.gapdatey 2.smokingstat15x#c.gapdatey

collect layout (colname#result result[p_e p_f p_g p_h]) (model)
collect style cell, nformat(%5.2f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.2f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style header result[p_e p_f p_g p_h], level(label)
collect label levels result p_e "p-value for chronic disease count" p_f "p-value for social class" p_g "p-value for educational attainment" p_h "p-value for smoking status"
collect style cell result [p_e p_f p_g p_h], nformat(%5.2f)
collect style showbase off
collect title "A priori model amyloid interaction associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export aprioritable2.docx, replace

*AMYLOID REF CHANGE TABLE 1
use "S:\LHA_MR1021\Amyloid, Volumetric & DTI x ACBS\2024\working set for upload.dta", clear
drop if mjrbrain_none!=1
mi set flong
mi register imputed chrondisease_69 disa_69 delir2 pp_69 apoe3 wmc_tot
mi impute chained (logit) disa_69 delir2 pp_69 apoe3 (ologit) chrondisease_69 (pmm, knn(5)) wmc_tot = brain_bsi2 hippoa2_bsi2 vent_bsi2 spm_tiv_vol1 cogchild smokingstat15x i46_edu gapdatey sex socialclass scanage, by(status_composite_wc_pvc_bl) rseed(270186) add(10) augment

collect clear
collect create cerbellaramyloidtable1

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wc_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(1)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa2_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wc_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(2)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wc_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(3)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey

collect layout (colname#result result[p_e]) (model)
collect style cell, nformat(%5.2f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.2f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style header result[p_e], level(label)
collect label levels result p_e "p-value for chronic disease count"
collect style cell result [p_e], nformat(%5.2f)
collect style showbase off
collect title "Cerebellar amyloid associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export cerbellaramyloidtable1.docx, replace

*AMYLOID REF CHANGE TABLE 2
collect clear
collect create cerbellaramyloidtable2

collect _r_b _r_ci _r_p, tag(model[(1)]): mi estimate: regress brain_bsi2 delir2##status_composite_wc_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(1)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(2)]): mi estimate: regress hippoa2_bsi2 delir2##status_composite_wc_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(2)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey
collect _r_b _r_ci _r_p, tag(model[(3)]): mi estimate: regress vent_bsi2 delir2##status_composite_wc_pvc_bl#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
collect p_e=r(p), tag(model[(3)]): mi test 1.chrondisease_69#c.gapdatey 2.chrondisease_69#c.gapdatey 3.chrondisease_69#c.gapdatey

collect layout (colname#result result[p_e]) (model)
collect style cell, nformat(%5.2f)
collect style cell result [_r_ci], sformat("(%s)") cidelimiter(", ")
collect style cell result [_r_p], nformat(%5.2f)
collect style cell border_block, border(right, pattern(nil))
collect levelsof cell_type
collect style cell cell_type[item column-header], halign(center)
collect style header result, level(hide)
collect style column, extraspace(1)
collect style row stack, spacer delimiter(" x ")
collect style header result[p_e], level(label)
collect label levels result p_e "p-value for chronic disease count"
collect style cell result [p_e], nformat(%5.2f)
collect style showbase off
collect title "Cerebellar amyloid associations between total brain, hippocampal and ventricular volume change (BSI) and delirium"

collect export cerbellaramyloidtable2.docx, replace


*-----------------------------------------------------GENERATE GRAPHS-----------------------------------------------------
*FOREST PLOT (FIGURE 1)
estimates drop _all
mi estimate: qui regress brain_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.gapdatey, nocons
estimates store Aa
mi estimate: qui regress hippoa2_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
estimates store Bb
mi estimate: qui regress vent_bsi2 delir2#c.gapdatey sex#c.gapdatey c.spm_tiv_vol1#c.gapdatey chrondisease_69#c.gapdatey pp_69#c.gapdatey c.wmc_tot#c.gapdatey status_composite_wm_pvc_bl#c.gapdatey apoe3#c.gapdatey c.scanage#c.gapdatey c.brain_bsi2#c.gapdatey c.gapdatey, nocons
estimates store Cc
estout Aa Bb Cc
coefplot Aa, drop(_cons gapdatey) xline(0) nolabels name(graph1)
coefplot Bb, drop(_cons gapdatey c.brain_bsi2#c.gapdatey) xline(0) nolabels name(graph2) 
coefplot Cc, drop(_cons gapdatey c.brain_bsi2#c.gapdatey) xline(0) nolabels name(graph3)
graph combine graph1 graph2 graph3
