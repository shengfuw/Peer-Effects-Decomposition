clear *
drop _all
set more off

********** Set path here **********
global DirectoryPath = "P:\AddHealth\Contract\24082201-Lin,MJ\Work\SF - Decomposing Peer Effects"
***********************************

cd "$DirectoryPath"
qui{
	capture mkdir do
	capture mkdir document
	capture mkdir rawdata
	capture mkdir workdata
	capture mkdir log
	capture mkdir output
}

global do = "$DirectoryPath/do"
global document = "$DirectoryPath/document"
global rawdata = "$DirectoryPath/rawdata"
global workdata = "$DirectoryPath/workdata"
global log = "$DirectoryPath/log"
global output = "$DirectoryPath/output"

program define distinct
	args var
	egen var_distinct = tag(`var')
	count if var_distinct
	drop var_distinct
end
