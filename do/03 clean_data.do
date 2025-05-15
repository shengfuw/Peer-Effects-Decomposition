* Author: Shengfu Wang
* The code was adapted with reference to: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/AJBJPM

cd "P:\AddHealth\Contract\24082201-Lin,MJ\Work\SF - Decomposing Peer Effects\do"
do "01 Initialize.do"
clear

cd "$workdata"
ls *.dta

*===============================
**## Clean Wave I inschool data
*===============================
use "$rawdata/inschool.dta", clear

*** Age ***
recode s1 (96/99=.), gen(age)
tab age, m

*** Sex ***
gen sex = s2
replace sex = 0 if missing(s2)


*** School grade ***
recode s3 (99=.), gen(grade)


*** Race ***
gen white = s6a 
gen black = s6b
gen asian = s6c
gen indian = s6d
gen other = s6e
gen hispanic = s4
recode hispanic (8=0)

egen multirace = rowtotal(white black indian asian other hispanic)

gen race = .
replace race = 1 if white == 1 & multirace == 1
replace race = 2 if black == 1 & multirace == 1
replace race = 3 if hispanic == 1
replace race = 4 if asian == 1 & multirace == 1
replace race = 5 if race == . & multirace > 0
replace race = . if multirace == 0


*** Family structure ***
gen family = .
replace family = 1 if s11 == 1 & s17 == 1 
replace family = 2 if s11 == 1 & s17 == 0 
replace family = 2 if s11 == 1 & missing(s17) 
replace family = 3 if s11 == 0 & s17 == 1 
replace family = 3 if missing(s11) & s17 == 1 
replace family = 4 if s11 == 0 & s17 == 0

gen family_two = family == 1
gen family_one = (family == 2 | family == 3)
gen family_other = (family != 1 & family != 2 & family != 3) 


*** US born ***
gen usborn = s8


*** Immigrant status ***
gen parent_foreign = 0 if !missing(s13) | !missing(s19)
replace parent_foreign = 1 if s13 == 0 | s19 == 0

gen secondG = 0
replace secondG = 1 if s13 == 0
replace secondG = 1 if s19 == 0
replace secondG = . if missing(s13) & missing(s19)

gen immigrant = .
replace immigrant = 1 if s8 == 0 & (s13 == 0 | s19 == 0) & family == 1
replace immigrant = 1 if s8 == 0 & s13 == 0 & family == 2
replace immigrant = 1 if s8 == 0 & s19 == 0 & family == 3

replace immigrant = 2 if s8 == 0 & (s13 == 0 | s19 == 0) & family == 1
replace immigrant = 2 if s8 == 1 & s13 == 0 & family == 2
replace immigrant = 2 if s8 == 1 & s19 == 0 & family == 3

replace immigrant = 3 if s8 == 1 & (s13 == 1 | s19 == 1) & family == 1
replace immigrant = 3 if s8 == 0 & s13 == 1 & family == 2
replace immigrant = 3 if s8 == 0 & s19 == 1 & family == 3

replace immigrant = 4 if s8 == 1 & family == 4


*** Parental eduction ***
recode s12 s18 (96/99=.)
gen ma_educ = s12
gen fa_educ = s18
recode ma_educ (1/6 10=0)(7/8=1)(9 11=.), gen(ma_college)
recode fa_educ (1/6 10=0)(7/8=1)(9 11=.), gen(fa_college)

recode ma_educ fa_educ (1=8)(2 3 4=12)(5=13)(6=14)(7=16)(8=18)(9=.)(10=0)(11=.)

egen pa_maxeduc = rowmax(ma_educ fa_educ)


*** Parental attachment ***
recode s16 s22 (7/9=.)
gen ma_care = s16
gen fa_care = s22
egen pa_avgcare = rowmean(ma_care fa_care)


*** School attachment ***
recode s62b s62e s62i (7/9=.)
gen s_att1 = 6-s62b
gen s_att2 = 6-s62e
gen s_att3 = 6-s62i
alpha s_att*, gen(s_attachment)


*** Other extra curriculars ***
gen club_activity = 1-s44
egen club_n = rowtotal(s44a*)


*** Depression ***
egen depress = rowmean(s60i s60j s60k s60l s60m s60n s60o)


*** GPA ***
recode s10* (1=4)(2=3)(3=2)(4=1)(5/99 = .)
gen gpa_english = s10a
gen gpa_math = s10b
gen gpa_history = s10c
gen gpa_science = s10d
egen gpa_overall = rowmean(gpa_*)
sum gpa_overall

sort sschlcde grade gpa_overall
bysort sschlcde grade: egen n = count(gpa_overall)
bysort sschlcde grade: egen gpa_overall_rank = rank(gpa_overall) , field
gen gpa_overall_pr = 100 - 100 * (gpa_overall_rank - 1) / (n - 1) if n > 1
drop n gpa_overall_rank


*** Save the cleaned data ***
destring sschlcde, replace
rename sschlcde scid
drop s1-s68 
gen scid_insch = scid
save "cleaned_inschool.dta", replace


*===============================
**## Clean Wave I inhome data
*===============================
use "$rawdata/wave1.dta", clear

*** Sex ***
gen female = (bio_sex == 2) if !missing(bio_sex)
tab female, m


*** Grade ***
recode h1gi20 (96/99=.), gen(grade) 
recode s3 (96/99=.), gen(grade_s)


*** Age *** 
gen age_y = (iyear - h1gi1y)
gen age_ym = 12*(iyear - h1gi1y) + (imonth-h1gi1m)
recode h1gi1m (96=.), gen(w1bmonth)
recode h1gi1y (96=.), gen(w1byear)
gen w1bdate = mdy(w1bmonth, 15, 1900+w1byear)
gen w1idate = mdy(imonth, iday, 1900+iyear)
format w1bdate %d
format w1idate %d
gen w1age = int((w1idate-w1bdate)/365.25)


*** Race ***
gen white = h1gi6a == 1 if !missing(h1gi6a)
gen black = h1gi6b == 1 if !missing(h1gi6b)
gen indian = h1gi6c == 1 if !missing(h1gi6c)
gen asian = h1gi6d == 1 if !missing(h1gi6d)
gen other = h1gi6e == 1 if !missing(h1gi6e)
gen hispanic = h1gi4 == 1 if !missing(h1gi4)

egen multirace = rowtotal(white black indian asian other)
tab multirace, m

recode h1gi8 (6/9=.)
gen race = .
replace race = 1 if white == 1 & multirace == 1
replace race = 2 if black == 1 & multirace == 1
replace race = 3 if indian == 1 & multirace == 1
replace race = 4 if asian == 1 & multirace == 1
replace race = 5 if other == 1 & multirace == 1
replace race = h1gi8 if multirace > 1
tab race, m

* Hispanic over-writting
recode race (1=1)(2=2)(3 5=4)(4=4), gen(race_h)
replace race_h = 3 if hispanic == 1
rename race race_s


*** Immigration status ***
/*
- First generation: foreign-born to foreign-born parents
- Second generation: US born to foreign-born parents
- Third or higher: US born to US born parents
*/
recode h1gi11 (6/9=.), gen(usborn)
replace usborn = 1 if h1gi3 == 0

gen uscome_y = h1gi13y - h1gi1y
gen uscome_ym = (h1gi13y - h1gi1y)*12 + h1gi13m - h1gi1m
gen uscome_6older = (usborn == 0) if !missing(usborn)
replace uscome_6older = 0 if uscome_y < 6

gen bioma_usborn = h1nm6
gen biofa_usborn = h1nf6
gen resma_usborn = h1rm2
gen resfa_usborn = h1rf2

gen ma_usborn = bioma_usborn
replace ma_usborn = resma_usborn if missing(bioma_usborn)
gen fa_usborn = biofa_usborn 
replace fa_usborn = resfa_usborn if missing(biofa_usborn)

gen immig_1 = . 
replace immig_1 = 1 if usborn == 0
replace immig_1 = 2 if usborn == 1 & (ma_usborn == 0 & fa_usbor == 0)
replace immig_1 = 2 if usborn == 1 & (ma_usborn == 1 & fa_usbor == 0)
replace immig_1 = 2 if usborn == 1 & (ma_usborn == 0 & fa_usbor == 1)
replace immig_1 = 3 if usborn == 1 & (ma_usborn == 1 & fa_usbor == 1)
tab immig_1, m

* parent's report on immigration status
recode pc1 (1/4=1)(9/12=1)(else = 0), gen(p_parents)
replace p_parents = . if missing(pc1)
gen p_usborn = pa3 == 1 if !missing(pa3) & p_parents == 1
gen immig_2 = immig_1
replace immig_2 = 2 if missing(immig_2) & usborn == 1 & p_usborn == 0
replace immig_2 = 3 if missing(immig_2) & usborn == 1 & p_usborn == 1


*** Public assistance ***
recode h1rm9 h1rf9 (6/9=.)
gen ma_pub_ass = h1rm9
gen fa_pub_ass = h1rf9
gen assistance = .
replace assistance = 0 if ma_pub_ass == 0
replace assistance = 0 if fa_pub_ass == 0
replace assistance = 1 if ma_pub_ass == 1
replace assistance = 1 if fa_pub_ass == 1
tab assistance, m


*** Fammily income ***
// gen family_inccome = pa55
recode pa55 (9996=.), gen(family_income)
gen family_income_ln = ln(family_income)


*** Fammily structure ***
local i = 1
foreach x of varlist h1hr3* {
	gen hrel`i' = `x'
	local i = `i' + 1
}

egen n_father = anycount(hrel*), values(11)
egen n_father_wife = anycount(hrel*), values(12)
egen n_father_partner = anycount(hrel*), values(13)
egen n_mother = anycount(hrel*), values(14)
egen n_mother_husband = anycount(hrel*), values(15)
egen n_mother_partner = anycount(hrel*), values(16)
recode n_father (1/2=1)
recode n_mother (1/2=1)

local i = 1
foreach x of varlist h1hr6* {
	gen prel`i' = `x'
	local i = `i' + 1
}

egen n_biofather = anycount(prel*), values(1)
egen n_adoptfather = anycount(prel*), values(3)
egen n_biomother = anycount(prel*), values(7)
egen n_adoptmother = anycount(prel*), values(9)
recode n_biofather (1/2=1)
recode n_biomother (1/2=1)

replace n_father = n_mother_husband if n_father == 0
gen n_nonbiofather = n_father 
replace n_nonbiofather = 0 if n_biofather == 1
replace n_mother = n_father_wife if n_mother == 0
gen n_nonbiomother = n_mother
replace n_nonbiomother = 0 if n_biomother == 1

gen famst14 = .
replace famst14 = 1 if n_biomother == 1 & n_biofather == 1 
replace famst14 = 2 if n_biomother == 1 & n_nonbiofather == 1 
replace famst14 = 3 if n_nonbiomother == 1 & n_biofather== 1 

replace famst14 = 4 if n_biomother == 1 & (n_father == 0 & n_adoptfather == 0 & n_mother_partner == 0)
replace famst14 = 5 if n_biofather == 1 & (n_mother == 0 & n_adoptmother == 0 & n_father_partner == 0)
replace famst14 = 6 if n_biomother == 1 & (n_father == 0 & n_adoptfather == 0 & n_mother_partner == 1)
replace famst14 = 7 if n_biofather == 1 & (n_mother == 0 & n_adoptmother == 0 & n_father_partner == 1)
replace famst14 = 8 if n_adoptmother == 1 & n_adoptfather == 1 & famst14 == .

replace famst14 = 9 if n_nonbiomother == 1 & n_father == 0 & famst14 == .
replace famst14 = 9 if n_father_partner == 1 & n_father == 0 & famst14 == .

replace famst14 = 10 if n_nonbiofather == 1 & n_mother == 0 & famst14 == .
replace famst14 = 10 if n_mother_partner == 1 & n_mother == 0 & famst14 == .

replace famst14 = 11 if n_nonbiomother == 1 & n_nonbiofather == 1 & famst14 == .

replace famst14 = 12 if n_nonbiomother == 1 & (n_father == 0 & n_adoptfather == 0 & n_mother_partner == 1)
replace famst14 = 13 if n_nonbiofather == 1 & (n_mother == 0 & n_adoptmother == 0 & n_father_partner == 1)
replace famst14 = 14 if famst14 == .

recode famst14 (1=1)(2 6=2)(3 7=3)(8=4)(11 12 13=5)(4 9=6)(5 10=7)(14=8), gen(famst8)
recode famst8 (4 5=4), gen(famst7)
recode famst7 (2 3 4=2), gen(famst5)
tab famst5, m


*** Parent's highest educational attainment ***
gen bioma_educ = h1nm4 if !missing(h1nm4)
gen biofa_educ = h1nf4 if !missing(h1nf4)
gen resma_educ = h1rm1 if !missing(h1rm1)
gen resfa_educ = h1rf1 if !missing(h1rf1)

* parent's reporting
recode pa12 pb8 (96/99=.) 
gen p_maeduc = pa12 if pc1 < 5 & !missing(pc1)
gen p_faeduc = pa12 if pc1 >= 9 & pc1 <= 12

* use child's report to impute missing
replace p_maeduc = pb8 if pc1 > 8 & pc1 <= 12 & !missing(pc1) & pb2 == 2
replace p_faeduc = pb8 if pc1 < 5 & !missing(pc1) & pc2 == 1

recode p_maeduc p_faeduc bioma_educ biofa_educ resma_educ resfa_educ (1=8)(2=11)(3 4 5=12)(6=13)(7=14)(8=16)(9=18)(10=0)(11 12 96/99=.)

replace p_maeduc = resma_educ if missing(p_maeduc)
replace p_faeduc = resfa_educ if missing(p_faeduc)
egen c_paeduc_max = rowmax(p_maeduc p_faeduc) 


*** Parental attachment ***
recode h1wp9 h1wp10 h1wp13 h1wp14 (6/9=.)
gen pa_att_closema = h1wp9
gen pa_att_carema = h1wp10
gen pa_att_closefa = h1wp13
gen pa_att_carefa = h1wp14

egen ma_attach = rowmean(pa_att_closema pa_att_carema)
egen fa_attach = rowmean(pa_att_closefa pa_att_carefa)
egen pa_attach_avg = rowmean(ma_attach fa_attach)
egen pa_attach_max = rowmax(ma_attach fa_attach)
egen pa_attach_min = rowmin(ma_attach fa_attach)

egen ma_attachs = rowtotal(pa_att_closema pa_att_carema)
egen fa_attachs = rowtotal(pa_att_closefa pa_att_carefa)
egen pa_attachs_avg = rowmean(ma_attachs fa_attachs)
egen pa_attachs_max = rowmax(ma_attachs fa_attachs)
egen pa_attachs_min = rowmin(ma_attachs fa_attachs)

recode s16 s22 (7 9=.)
alpha s16 s22, gen(p_attachment)
lab var p_attachment "p_attachment"


*** School attachment ***
recode s62b s62e s62l h1wp14 (6/9=.)
gen s_att1 = 6-s62b
gen s_att2 = 6-s62e
gen s_att3 = 6-s62i
alpha s_att*, gen(s_attachment)
sum s_attachment


*** Sibling size ***
recode h1hr14 (96/99=.)
gen sibsize = h1hr14
recode sibsize (5/max=5), gen(sibsize5)


*** PVT test score ***
gen pvt = ah_pvt
gen m_pvt = (1 == missing(pvt))
gen pvt_raw = ah_raw


*** Region ***
recode h1ir12 (96/99=.), gen(region)
gen rural = region == 1 if !missing(region)
tab region, m


*** Depression ***
recode h1fs1-h1fs19 (6/9=.)
gen dep_bother = h1fs1
gen dep_eating = h1fs2
gen dep_blues = h1fs3
gen dep_good_rev = 3-h1fs4
gen dep_keepmind = h1fs5
gen dep_depress = h1fs6
gen dep_tired = h1fs7
gen dep_hope_rev = 3-h1fs8
gen dep_failure = h1fs9
gen dep_fearful = h1fs10
gen dep_happy_rev = 3-h1fs11
gen dep_enjoy_rev = 3-h1fs15
gen dep_sad = h1fs16
gen dep_start = h1fs18
gen dep_life = h1fs19

egen cesd = rowtotal(dep_*)

gen dep_talkless = h1fs12
gen dep_lonely = h1fs13
gen dep_unfriend = h1fs14
gen dep_dislike = h1fs17
egen fcesd = rowtotal(dep_*)

for var dep_*: replace cesd = . if missing(X)
for var dep_*: replace fcesd = . if missing(X)


*** GPA ***
recode h1ed11 h1ed12 h1ed13 h1ed14 (1=4)(2=3)(3=2)(4=1)(5/99 = .)
gen gpa_english = h1ed11
gen gpa_math = h1ed12
gen gpa_history = h1ed13
gen gpa_science = h1ed14
egen gpa_overall = rowmean(gpa_*)
sum gpa_*

sort scid grade gpa_overall
bysort scid grade: egen n = count(gpa_overall)
bysort scid grade: egen gpa_overall_rank = rank(gpa_overall) , field
gen gpa_overall_pr = 100 - 100 * (gpa_overall_rank - 1) / (n - 1) if n > 1
drop n gpa_overall_rank


*** Educational expectation ***
recode h1ee1 (6/9=.), gen(educexp_want)
recode h1ee2 (6/9=.), gen(educexp_likely)
sum educexp_*


*** Percieved parental expectation ***
recode h1wp15 (6/9=.), gen(disappoint_dad_college)
recode h1wp16 (6/9=.), gen(disappoint_dad_highSch)
recode h1wp11 (6/9=.), gen(disappoint_mom_college)
recode h1wp12 (6/9=.), gen(disappoint_mom_highSch)


*** Skip school without excuse
recode h1ed2 (996/999=.), gen(skip_school)


*** Hang out with friends
recode h1da7 (6/9=.), gen(hang_out_with_friends)


*** Do parents let R make own decisions about the people hang around with
recode h1wp2 (6/9=.), gen(hang_out_own_decision)


*** Feel that friends care about
recode h1pr4 (6/98=.), gen(feel_friends_care)


*** Save the cleaned data ***
drop h1gi1m-smp08 
keep aid scid sscid iyear imonth female-feel_friends_care
foreach x of varlist *{
	if "`x'" != "aid" & "`x'" != "scid" &  "`x'" != "sscid" {
		rename `x' `x'_1
	}
}
gen scid_1 = scid
save "cleaned_wave1.dta", replace


*===============================
**## Clean Wave II inhome data
*===============================
use "$rawdata/wave2.dta", clear


*** Sex ***
gen female = bio_sex2 == 2 if !missing(bio_sex2)


*** Same-school attendance ***
recode h2gi7 h2gi8 (6/9=.)
gen same_school = h2gi7
gen same_sschool = h2gi8 


*** Grade ***
recode h2gi9 (13 14 96/99=.), gen(grade)


*** GPA ***
recode h2ed7 h2ed8 h2ed9 h2ed10 (1=4)(2=3)(3=2)(4=1)(5/99 = .)
gen gpa_english = h2ed7
gen gpa_math = h2ed8
gen gpa_history = h2ed9
gen gpa_science = h2ed10
egen gpa_overall = rowmean(gpa_*)
sum gpa_*

sort scid grade gpa_overall
bysort scid grade: egen n = count(gpa_overall)
bysort scid grade: egen gpa_overall_rank = rank(gpa_overall) , field
gen gpa_overall_pr = 100 - 100 * (gpa_overall_rank - 1) / (n - 1) if n > 1
drop n gpa_overall_rank


*** Educational expectation ***
recode h2ee1 (6/99=.), gen(educexp_want)
recode h2ee2 (6/99=.), gen(educexp_likely)
sum educexp_*


*** Future expectation ***
recode h2ee12-h2ee17 (6/9=.)
gen exp_live35 = h2ee12 
gen exp_marry25 = h2ee13
gen exp_killed21 = h2ee14
gen exp_hiv = h2ee15
gen exp_graduate = h2ee16
gen exp_income30 = h2ee17


*** Percieved parental expectation ***
recode h2wp15 (6/9=.), gen(disappoint_dad_college)
recode h2wp16 (6/9=.), gen(disappoint_dad_highSch)
recode h2wp11 (6/9=.), gen(disappoint_mom_college)
recode h2wp12 (6/9=.), gen(disappoint_mom_highSch)


*** Hang out with friends
recode h2da7 (6/9=.), gen(hang_out_with_friends)


*** Do parents let R make own decisions about the people hang around with
recode h2wp2 (6/9=.), gen(hang_out_own_decision)


*** Feel that friends care about
recode h2pr4 (6/98=.), gen(feel_friends_care)


*** Depression ***
recode h2fs1-h2fs19 (6/8=.)
gen dep_bother = h2fs1
gen dep_eating = h2fs2
gen dep_blues = h2fs3
gen dep_good_rev = 3-h2fs4
gen dep_keepmind = h2fs5
gen dep_depress = h2fs6
gen dep_tired = h2fs7
gen dep_hope_rev = 3-h2fs8
gen dep_failure = h2fs9
gen dep_fearful = h2fs10
gen dep_happy_rev = 3-h2fs11
gen dep_enjoy_rev = 3-h2fs15
gen dep_sad = h2fs16
gen dep_start = h2fs18
gen dep_life = h2fs19

for var dep_*: replace X = . if X == -3 | X == 6
egen cesd = rowtotal(dep_*)

gen dep_talkless = h2fs12
gen dep_lonely = h2fs13
gen dep_unfriend = h2fs14
gen dep_dislike = h2fs17

for var dep_*: replace X = . if X == -3 | X == 6
egen fcesd = rowtotal(dep_*)

for var dep_*: replace cesd = . if missing(X)
for var dep_*: replace fcesd = . if missing(X)


*** Save the cleaned data ***
drop h2gi1m-h2ir31 
keep aid scid2 sscid2 iyear imonth female-fcesd
foreach x of varlist *{
	if "`x'" != "aid" & "`x'" != "scid2" &  "`x'" != "sscid2" {
		rename `x' `x'_2
	}
}
gen scid_2 = scid
save "cleaned_wave2.dta", replace
ls *.dta


* Add some Wave 1 and 2 variables to inschool 
use "cleaned_inschool", clear
local allvars `r(varlist)'
merge m:1 aid using "cleaned_wave1.dta", keepusing(`allvars' educexp_* gpa_overall_1) keep(master match) nogen force
merge m:1 aid using "cleaned_wave2.dta", keepusing(`allvars' educexp_* gpa_overall_2) keep(master match) nogen force
save "cleaned_inschool", replace


**##  Merge all cleaned data
*===============================
cd "$workdata"
ls *.dta

use "cleaned_inschool.dta", clear // n = 90118

* Merge with Wave I
merge m:1 aid using "cleaned_wave1.dta", gen(_merge_with_w1) force // 95508
merge m:1 aid using "$rawdata/weights1.dta", nogen // n = 95508


* Merge with Wave II
merge m:1 aid using "cleaned_wave2.dta", gen(_merge_with_w2) force // 95509
merge m:1 aid using "$rawdata/weights2.dta", nogen // n = 99509

save "cleaned_all_wave.dta", replace 


* Merge with School Info
use "$rawdata/schinfo.dta", clear
destring scid, replace
keep scid inhome inschool n_roster n_inschl grades sat_schl
foreach x of varlist *{
	if "`x'" != "scid" {
		rename `x' sch_`x'
	}
}
save "Other/schinfo.dta", replace

use "cleaned_all_wave.dta", clear
merge m:1 scid using "Other/schinfo.dta"
drop if _merge == 2
drop _merge
save "cleaned_all_wave.dta", replace


* Merge with In-School Network (Constructed)
use "$rawdata/network.dta", clear
keep aid idgx2 odgx2 bcent10x reach igdmean esden nes erden ner esrden nesr ehsgrd ehrgrd errngrd ehgrd erngrd
foreach x of varlist *{
	if "`x'" != "aid" {
		rename `x' net_`x'
	}
}
save "Other/network.dta", replace


* Merge with friendship nomination
use "$rawdata/friend_inschool", clear
foreach f of varlist mf*aid ff*aid {
	replace `f' = . if `f' == 55555555 // partner?
	replace `f' = . if `f' == 77777777
	replace `f' = . if `f' == 88888888
	replace `f' = . if `f' == 99999999
}
order aid
save "Other/friend_inschhol_cleaned.dta", replace


use "cleaned_all_wave.dta", clear
merge m:1 aid using "Other/network.dta"
drop if _merge == 2
drop _merge
save "cleaned_all_wave.dta", replace
clear


ls *.dta
use "cleaned_all_wave.dta", clear
list grade grade_1 grade_2 gpa_overall  gpa_overall_1 gpa_overall_2 educexp_want_1  educexp_want_2 educexp_likely_1 educexp_likely_2 if sqid != "" & gswgt1 != . & gswgt2 != .

