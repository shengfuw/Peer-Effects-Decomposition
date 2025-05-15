* Author: Shengfu Wang

********** Set path here **********
cd "P:\AddHealth\Contract\24082201-Lin,MJ\Work\SF - Decomposing Peer Effects\do"
***********************************

clear all
do "01 Initialize.do"

cd "$workdata"
ls *.dta
drop _all

cap: program drop DefineVar 
program DefineVar

	global treatment "peer_treatment_mean" 
	
	global outcome "gpa_overall_2"
	
	global mediator1 "educexp_want_1" 
	
	global mediator2 "educexp_likely_1"
	
	global cv grade female race immig family pa_educ assistance sibsize pvt treatment_0 n_peer ///
	p_attachment s_attachment region family_income_ln fcesd_1 club_n skip_school hang_out_own_decision_1 ///
	net_esrden net_bcent10x net_reach_ln

	global peercv female_peermean black_peermean hispanic_peermean other_peermean immig_1st_peermean immig_2nd_peermean family_one_peermean family_other_peermean pa_educ_peermean
	
	global post_cv educexp_want_1_peermean educexp_likely_1_peermean
end

cap: program drop ProcessVar 
program ProcessVar
quietly {
	** Some basic pre-process
	rename gpa_overall_pr treatment_0
	rename N_peer n_peer
	
	lab var parent_attachment "parent_attachment"
	lab var grade_x "grade"
	
	gen net_reach_ln = ln(net_reach)
	
	local percent_vars female_peermean white_peermean black_peermean hispanic_peermean other_peermean immig_1st_peermean immig_2nd_peermean immig_3rd_peermean family_two_peermean family_one_peermean family_other_peermean
	foreach X of local percent_vars { 
		replace `X' = `X' * 100 
	}
	
	* Remove copyed variables
	drop scid_x
	drop *_y
	rename *_x *
	
	* Recode categorical variables
	cap: drop race
	gen race = .
	replace race = 1 if white == 1
	replace race = 2 if black == 1
	replace race = 3 if hispanic == 1
	replace race = 4 if other == 1
	lab define racelbl 1"white" 2"black" 3"hispanic" 4"other" 
	lab values race racelbl
	
	cap: drop immig
	gen immig = .
	replace immig = 1 if immig_1st == 1
	replace immig = 2 if immig_2nd == 1
	replace immig = 3 if immig_3rd == 1
	lab define immiglbl 1"immig_1st" 2"immig_2nd" 3"immig_3rd" 
	lab values immig immiglbl
	
	cap: drop family
	gen family = .
	replace family = 1 if family_two == 1
	replace family = 2 if family_step == 1
	replace family = 3 if family_one == 1
	replace family = 4 if family_other == 1
	lab define familylbl 1"family_two" 2"family_step" 3"family_one" 4"family_other" 
	lab values family familylbl
	
	* Sample Exclusion Criteria
	replace grade = . if grade <= 6 | grade >= 13 	
	
	gen out_sample = 0 if !missing(gswgt2) & !missing(gswgt1)
	replace out_sample = 1 if out_sample == 0 & sqid == ""
	replace out_sample = 2 if out_sample == 0 & missing(grade)
	**Missing on Treatment:
	replace out_sample = 3 if out_sample == 0 & missing(peer_treatment_mean) 
	**Missing on Outcome:
	replace out_sample = 4 if out_sample == 0 & missing(gpa_overall_2)
	**Missing on Mediator: ?
	
	gen subgr = (out_sample == 0) if !missing(out_sample)
	
	* Keep only used variables
	// merge 1:m aid using "cleaned_all_wave.dta", keep(master match) nogen force
	
	keep aid scid gswgt1 gswgt2 $treatment $outcome  /// 
	 $mediator1 $mediator2 $cv $peercv $post_cv *_peermean educexp_* out_sample subgr
	
	order aid scid gswgt1 gswgt2 $treatment $outcome  /// 
	 $mediator1 $mediator2 educexp_* $cv $peercv $post_cv *_peermean 
}

tab out_sample
tab subgr

keep if subgr == 1
drop out_sample subgr
end


cap: program drop MultipleImpute
program MultipleImpute
mi set flong
mi xtset, clear

qui{
misstable sum $treatment $outcome  /// 
	 $mediator1 $mediator2 $cv $peercv $post_cv
	 
mi register imputed ///
	 $treatment $outcome  /// 
	 $mediator1 $mediator2 $cv $peercv $post_cv

mi impute chained ///
	(pmm, knn(5))  $mediator1 $mediator2 pa_educ sibsize pvt treatment_0 n_peer p_attachment s_attachment family_income_ln fcesd_1 club_n skip_school net_esrden net_bcent10x net_reach_ln $peercv $post_cv ///
	(logit, augment) female assistance hang_out_own_decision ///
	(mlogit, augment) grade race immig family region /// 
	, add(30) rseed(123) dots 
}
end


cap: program drop PullSample
program PullSample
args peer_type
display "Processing `peer_type':"

use "reg_`peer_type'_all_exclude_self.dta", clear

DefineVar
ProcessVar
save "$workdata/sample_`peer_type'_MW2.dta", replace

MultipleImpute
save "$workdata/sample_mi_`peer_type'_MW2.dta", replace

end

*============================
**# Process and Export Fata
*============================

* Main analysis:
PullSample "friend_all" 


* Comparing different peer type:
foreach peer_type in grade club coursemate friend_in friend_out friend_both {
	PullSample `peer_type'
}


*============================
**# Table 1 (Descriptive)
*============================
ls
DefineVar

use "$workdata/sample_mi_friend_all.dta", clear
drop if _mi_m == 0
local cv_cat "grade female race immig family"
loca cv $cv
local cv_conti: list cv - cv_cat
dtable $treatment $outcome  /// 
	 $mediator1 $mediator2 ///
	 i.(`cv_cat') `cv_conti' ///
	 female_peermean-educexp_likely_1_peermean $post_cv [aw=gswgt2],  ///
	 continuous($outcome $treatment $mediator1 $mediator2 `cv_conti' $peercv $post_cv, stat(count mean sd min p25 p50 p75 max)) nformat(%12.2fc mean sd min max) novarlab ///
	export("Table1_MI.pdf", replace) 
!move "Table1_MI.pdf" "$output/Table1_MI.pdf"


use "$workdata/sample_friend_all.dta", clear
local cv_cat "grade female race immig family"
loca cv $cv
local cv_conti: list cv - cv_cat
dtable $treatment $outcome  /// 
	 $mediator1 $mediator2 ///
	 i.(`cv_cat') `cv_conti' ///
	 female_peermean-educexp_likely_1_peermean $post_cv [aw=gswgt2],  ///
	 continuous($outcome $treatment $mediator1 $mediator2 `cv_conti' $peercv $post_cv, stat(count mean sd min p25 p50 p75 max)) nformat(%12.2fc mean sd min max) novarlab ///
	export("Table1.pdf", replace) 
!move "Table1.pdf" "$output/Table1.pdf"


*============================
**# Total Effects 
*============================
mi estimate: areg $outcome $treatment $cv $peercv [pw=gswgt2], absorb(scid)

