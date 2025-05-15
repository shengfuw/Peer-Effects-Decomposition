
********** Set path here **********
cd "P:\AddHealth\Contract\24082201-Lin,MJ\Work\SF - Decomposing Peer Effects\do"
global SASFile = "P:\AddHealth\Contract\24082201-Lin,MJ\Data"
********** Set path here **********

do "01 initialize.do"
clear

****** Convert raw SAS file to Stata file
cd "$rawdata"

// Wave I - In-School
import sasxport5 "$SASFile\Core Files - Wave I\Wave I In-School Questionnaire Data\Inschool.xpt"
save "inschool.dta", replace

// Wave I - Core
import sasxport5 "$SASFile\Core Files - Wave I\Wave I In Home Interview Data\allwave1.xpt"
save "wave1.dta", replace

// Wave I - Weights
import sasxport5 "$SASFile\Core Files - Wave I\Wave I In-Home Weights\Homewt1.xpt"
save "weights1.dta", replace

// Wave II - Core
import sasxport5 "$SASFile\Core Files - Wave II\Wave II In Home Interview Data\wave2.xpt"
save "wave2.dta", replace

// Wave II - Weights
import sasxport5 "$SASFile\Core Files - Wave II\Wave II Grand Sample Weights\Homewt2.xpt"
save "weights2.dta", replace

// Network - In-School 
import sasxport5 "$SASFile\Friendship Files\Wave I In-School Friendship Nominations\sfriend.xpt"
save "friend_inschool_raw.dta", replace

drop if sqid == "999999" 
merge 1:m sqid using "inschool.dta"
drop if _merge == 2
keep *aid*
order aid
save "friend_inschool.dta", replace

// Network - Wave I
import sasxport5 "$SASFile\Friendship Files\Wave I In-Home Friendship Nominations\hfriend1.xpt"
forvalues i = 1/5 {
	rename mf_aid`i' mf`i'aid
	rename ff_aid`i' ff`i'aid
}
drop dat
save "friend_w1.dta", replace

// Network - Wave II
import sasxport5 "$SASFile\Friendship Files\Wave II In-Home Friendship Nominations\hfriend2.xpt"
forvalues i = 1/5 {
	rename mf_aid`i' mf`i'aid
	rename ff_aid`i' ff`i'aid
}
save "friend_w2.dta", replace

// Network - In-School (Constructed)
import sasxport5 "$SASFile\Network Files\Wave I School Network Data\network.xpt"
save "network.dta", replace

// School information
import sasxport5 "$SASFile\Core Files - Wave I\School Information Data\Schinfo.xpt"
save "schinfo.dta", replace

// Transcript (Wave III) - Course Networks
import sasxport5 "$SASFile\Wave III Education Files\Wave III Academic Networks\edunet.xpt"
save "edu_network.dta", replace


clear
ls *.dta