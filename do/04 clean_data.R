# Author: Shengfu Wang
# The code was adapted with reference to: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/AJBJPM

####### Load packages and set file path #######
packages = c("rio", "here", "dplyr", "data.table", "fst", "igraph", "Matrix", "car", "lfe", "expss", "survey", "stargazer")
invisible(lapply(packages, library, character.only = T))

workdata_path = "P:/AddHealth/Contract/24082201-Lin,MJ/Work/SF - Decomposing Peer Effects/workdata"
rawdata_path = "P:/AddHealth/Contract/24082201-Lin,MJ/Work/SF - Decomposing Peer Effects/rawdata"
do_path = "P:/AddHealth/Contract/24082201-Lin,MJ/Work/SF - Decomposing Peer Effects/do"

setwd(workdata_path)
dir()
memory.limit(size=20000)

source(file.path(do_path, "self_defined_functions.R"))


############## Set variables ##############
treatment_var = c("gpa_overall")
peer_controls = c("female",  "white", "black", "hispanic", "other", "immig_1st", "immig_2nd", "immig_3rd", 
                  "family_two", "family_one", "family_other", "pa_educ",
                  "educexp_want_1", "educexp_likely_1", "educexp_want_2", "educexp_likely_2", "gpa_overall_1", "gpa_overall_2")


############## Import data #############
raw_data_inschool = data.table(import(file.path(rawdata_path, "inschool.dta")))

processed_data_inschool = data.table(import(file.path(workdata_path, "cleaned_inschool.dta")))
processed_data_wave1 = data.table(import(file.path(workdata_path, "cleaned_wave1.dta")))
processed_data_wave2 = data.table(import(file.path(workdata_path, "cleaned_wave2.dta")))

weight_wave1 = data.table(import(file.path(rawdata_path, "weights1.dta")))
weight_wave2 = data.table(import(file.path(rawdata_path, "weights2.dta")))

friendship_inschool = data.table(import(file.path(rawdata_path, "friend_inschool_raw.dta")))
friendship_wave1 = data.table(import(file.path(rawdata_path, "friend_w1.dta")))
friendship_wave2 = data.table(import(file.path(rawdata_path, "friend_w2.dta")))
friednship_coursemate = data.table(import(file.path(rawdata_path, "edu_network.dta")))

network = data.table(import(file.path(workdata_path, "Other", "network.dta")))


####### Additional cleaning #######
raw_data_inschool[aid == "", aid := as.character(10000000+seq(.N))] # Impute arbitrary aid into in-school data
processed_data_inschool = process_inschool(processed_data_inschool)
treatment_data = extract_treatment_data(processed_data_inschool, treatment_var)


####### Create edge lists #######
edgelist_grade = edge_peer_grade(data = raw_data_inschool)
edgelist_club = edge_peer_club(data = raw_data_inschool)
edgelist_coursemate = edge_peer_coursemate(data = friednship_coursemate, filter = "all", type = "U|re")
edgelist_friendship_all = edge_peer_friendship(friendship_data = friendship_inschool, inschool_data = raw_data_inschool, exclude_self = T, same_sex = "all", direction = "all")
edgelist_friendship_in = edge_peer_friendship(friendship_data = friendship_inschool, inschool_data = raw_data_inschool, exclude_self = T, same_sex = "all", direction = "in")
edgelist_friendship_out = edge_peer_friendship(friendship_data = friendship_inschool, inschool_data = raw_data_inschool, exclude_self = T, same_sex = "all", direction = "out")
edgelist_friendship_both = edge_peer_friendship(friendship_data = friendship_inschool, inschool_data = raw_data_inschool, exclude_self = T, same_sex = "all", direction = "both")

list_peer_type = list()
list_peer_type[[1]] = list(edgelist = edgelist_grade, peer_name = "grade")
list_peer_type[[2]] = list(edgelist = edgelist_club, peer_name = "club")
list_peer_type[[3]] = list(edgelist = edgelist_friendship_all, peer_name = "friend_all")
list_peer_type[[4]] = list(edgelist = edgelist_friendship_in, peer_name = "friend_in")
list_peer_type[[5]] = list(edgelist = edgelist_friendship_out, peer_name = "friend_out")
list_peer_type[[6]] = list(edgelist = edgelist_friendship_both, peer_name = "friend_both")
list_peer_type[[7]] = list(edgelist = edgelist_coursemate, peer_name = "coursemate")


####### Pull data #######
### condition 0: different peer types
m_filter = "nofilter"
m_reference = "all"
m_selfinclude = "exclude_self" # "include_self"
peer_type_i = c(1:length(list_peer_type))  
pull_data(list_peer_type, peer_type_i, m_filter, m_reference, m_selfinclude)


### condition 1: different reference for calculating quantiles
m_filter = "nofilter"
m_selfinclude = "exclude_self"
peer_type_i = 3 # only: friend_all
for (m_reference in c("grade", "school")) {
  pull_data(list_peer_type, peer_type_i, m_filter, m_reference, m_selfinclude)
}

cat("Done!")
