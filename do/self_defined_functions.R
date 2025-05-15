
## 1. Preprocess
process_inschool = function(inschool_data){
  inschool_data[, `:=`(
    parent_usborn = 1 - parent_foreign,
    self_usborn = usborn, 
    pa_educ = pa_maxeduc,
    immigration = car::recode(immigrant, c('4=3'))
  )]
  inschool_data[, `:=`(
    immig_1st = as.integer(immigration == 1),
    immig_2nd = as.integer(immigration == 2),
    immig_3rd = as.integer(immigration == 3),
    family_two = as.integer(family == 1), 
    family_one = as.integer(family %in% c(2, 3)),
    family_other = as.integer(family %in% c(1, 2, 3) == FALSE)
  )]
  inschool_data[sex != 0, female := as.integer(sex == 2)]

  return(inschool_data)
}

extract_treatment_data = function(data, treatment_var){
  data = data[grade >= 7 & grade <= 12, c("aid", "scid", "grade", treatment_var), with = F]
  data$treatment <- data[[ncol(data)]]
  return(data)
}


## 2. Create different types of edge list
edge_peer_grade <- function(data){
  
  data[s3 >= 7 & s3 <= 12, grade := s3]
  data[grade == 13 | grade == 6, grade := NA]
  data[, scid := as.numeric(sschlcde)]
  data = data[!is.na(grade) & !is.na(scid), ]
  
  setorder(data, scid, grade)
  data[, scid_grade := paste0(scid, ":", grade)]
  
  scdata = data[, c('aid', 'scid_grade')]
  scdata = scdata[scdata$aid != '' & scdata$scid_grade != '', ]
  
  graph_data = scdata[, .(edgelist = list(create_full_edge(aid))), by = scid_grade]
  all_edgelist = rbindlist(graph_data[, edgelist])
  names(all_edgelist) <- c("aid", "peer_aid")
  return(all_edgelist)
}

edge_peer_club <- function(data){
  
  data = data[, c("aid", "scid", grep("s44", names(data), value = T)), with = F]
  data = data[s44 == 0, -"s44"]
  data = data[aid != '', ]
  
  setorder(data, scid, aid)
  graph_data = data[, .(edge_list = list(create_twomode_edge(aid, .SD))), .SDcols=paste0("s44a", 1:33), by = scid]
  
  edgelist = rbindlist(graph_data[, edge_list])
  names(edgelist) <- c("aid", "peer_aid")
  return(edgelist)
}


edge_peer_coursemate <- function(data, filter = 94, type = "W|re", course_group = NULL){
  names(data) = tolower(names(data))
  
  if(filter != "all"){
    data = data[enyrlp == filter, ]
  }
  
  data[, scid := abs(as.numeric(ensclid))]
  
  if (type == "W|re") group_var = "encombcw"
  if (type == "U|re") group_var = "encombcu"
  if (type == "W|one") group_var = "enclustw"
  if (type == "U|one") group_var = "enclustu"
  data[, group := get(group_var)]
  data = data[!is.na(group) & !is.na(scid), ]
  
  setorder(data, scid, group)
  data[, scid_group := paste0(scid, ":", group)]
  
  scdata = data[, c("aid", "scid_group")]
  scdata = scdata[aid != "" & scid_group != "", ]
  scdata = unique(scdata)
  
  graph_data = scdata[, .(edge_list = 
                            list(create_full_edge(aid))), by = scid_group]
  
  all_edgelist = rbindlist(graph_data[, edge_list])
  names(all_edgelist) <- c("aid", "peer_aid")
  
  if (is.null(course_group)) {
    return(all_edgelist)
  } else {
    scdata = data[, c("aid", "scid", "scid_group")]
    scdata = scdata[aid != "" & scid_group != "", ]
    scdata = unique(scdata)
    return(scdata)
  }
}

edge_peer_friendship <- function(friendship_data, inschool_data, direction="out", same_sex="all", exclude_self=TRUE){
  
  female_data = inschool_data[inschool_data$aid != "", c("aid", "s2")]
  female_data[, female := ifelse(s2 == 2, 1, 0)]
  female_data[, s2 := NULL]
  
  names(friendship_data) = tolower(names(friendship_data))
  edgelist = melt(friendship_data, measure = list(paste0("mf",1:5,"aid"), paste0("ff",1:5,"aid")),
                  value.name = c("mfaid", "ffaid"), variable.name = "order")
  edgelist = melt(edgelist, measure = list(c("mfaid", "ffaid")), value.name = "aid", variable.name = "gender")
  names(edgelist) = c("sqid", "order", "gender", "peer_aid")
  edgelist = edgelist[sqid != 999999, ]
  edgelist = na.omit(edgelist)
  
  
  edgelist = merge(x = edgelist, y = inschool_data[, c("sqid", "aid")], by = "sqid", all.x = T)
  
  invalid_codes = c(77777777, 88888888, 99999999, 99959995)
  edgelist = edgelist[!(peer_aid %in% invalid_codes), ]
  edgelist[, peer_aid := as.character(peer_aid)]
  
  edgelist = merge(x = edgelist, y = female_data, by.x = "aid", by.y = "aid", all.X = T)
  edgelist = merge(x = edgelist, y = female_data, by.x = "peer_aid", by.y = "aid", all.X = T)
  
  edgelist[, same_sex := as.integer(female.x == female.y)]
  edgelist = edgelist[, c('aid', 'peer_aid', 'same_sex')]
  
  # exclude self-nomination
  if (exclude_self == TRUE) {
    edgelist = edgelist[aid != peer_aid, ]
  }
  
  # filter by same-sex (T) or opposite-sex (F) friendship
  if (same_sex == TRUE) {
    edgelist = edgelist[same_sex == 1, ]
  } else if (same_sex == FALSE) {
    edgelist = edgelist[same_sex == 0, ]
  } else {
    # same_sex = "all": nothing happens
  }
  
  edgelist = edgelist[, c('aid', 'peer_aid')]
  
  # consider friendship direction
  if (direction == "out"){
    # nothing happens
  } else if (direction == "in") {
    names(edgelist) = c('peer_aid', 'aid')
  } else if (direction == "all") {
    edgelist_in = edgelist
    names(edgelist) = c('peer_aid', 'aid')
    edgelist = rbind(edgelist, edgelist_in)
  } else if (direction == "both") {
    edgelist_in = edgelist
    names(edgelist) = c('peer_aid', 'aid')
    edgelist = rbind(edgelist, edgelist_in)
    edgelist[, n_tie := .N, by = c("aid", "peer_aid")]
    edgelist = unique(edgelist[n_tie == 2, ])
    edgelist[, n_tie := NULL]
  } else {
    stop("Please specify frienship direction type!")
  }
  
  return(edgelist)
  
}

create_full_edge <- function(ids) {
  n = length(ids)
  g = make_full_graph(n, directed = T)
  V(g)$name = ids
  return(data.table(as_edgelist(g)))
}

create_twomode_edge <- function(ids, dt){
  dt = as.matrix(dt)
  rownames(dt) = ids
  tmp <- as(dt, "dgTMatrix")
  edges <- data.frame(P=rownames(dt)[tmp@i + 1], Q=colnames(dt)[tmp@j + 1])
  g = graph_from_data_frame(edges)
  V(g)$type = bipartite_mapping(g)$type
  g = bipartite_projection(g, which = F)
  return(data.table(as_edgelist(g)))
}

## 3. Create peer-level controls and treatment

measure_peer_mean <- function(edgelist, processed_data, control.varlist, include_self=F) {
  
  # merge with attribute data
  attr_data = processed_data[, c("aid", control.varlist), with = FALSE]
  
  edgelist = edgelist[, c("aid", "peer_aid")]
  
  if (include_self == T){
    edgelist_aid = unique(edgelist[,aid])
    edgelist = rbind(edgelist, data.table(aid=edgelist_aid, peer_aid=edgelist_aid))
  }
  
  edgelist = merge(x = edgelist, y = attr_data, by.x = c("peer_aid"), by.y = c("aid"), all.x = T)
 
   # how to deal with friends' missing values?
  peer_mean <- edgelist[, sapply(.SD, function(x) list(mean = mean(x, na.rm = T))),
                        .SDcols = control.varlist, by = "aid"]
  
  return(peer_mean)
}

measure_peer_distribution <- function(edgelist, treatment_data, include_self = "include_self", filter="filter", reference="all") {
  # calculate quantiles
  if (filter == "filter") {
    subsample_aids = unique(unlist(edgelist[, c("aid", "peer_aid")]))
    filtered_row = treatment_data$aid %in% subsample_aids
  } else {
    filtered_row = 1:nrow(treatment_data)
  }
  
  if (reference == "all") {
    treatment_data[filtered_row, `:=`(
      qt05 = quantile(treatment, .05, type = 7, na.rm = T),
      qt10 = quantile(treatment, .10, type = 7, na.rm = T),
      qt15 = quantile(treatment, .15, type = 7, na.rm = T),
      qt20 = quantile(treatment, .20, type = 7, na.rm = T),
      qt25 = quantile(treatment, .25, type = 7, na.rm = T),
      qt90 = quantile(treatment, .90, type = 7, na.rm = T),
      qt95 = quantile(treatment, .95, type = 7, na.rm = T)
    )]
  } else if (reference == "school") {
    treatment_data[filtered_row, `:=`(
      qt05 = quantile(treatment, .05, type = 7, na.rm = T),
      qt10 = quantile(treatment, .10, type = 7, na.rm = T),
      qt15 = quantile(treatment, .15, type = 7, na.rm = T),
      qt20 = quantile(treatment, .20, type = 7, na.rm = T),
      qt25 = quantile(treatment, .25, type = 7, na.rm = T),
      qt90 = quantile(treatment, .90, type = 7, na.rm = T),
      qt95 = quantile(treatment, .95, type = 7, na.rm = T)
    ), by = "scid"]
  } else if (reference == "grade") {
    treatment_data[filtered_row, `:=`(
      qt05 = quantile(treatment, .05, type = 7, na.rm = T),
      qt10 = quantile(treatment, .10, type = 7, na.rm = T),
      qt15 = quantile(treatment, .15, type = 7, na.rm = T),
      qt20 = quantile(treatment, .20, type = 7, na.rm = T),
      qt25 = quantile(treatment, .25, type = 7, na.rm = T),
      qt90 = quantile(treatment, .90, type = 7, na.rm = T),
      qt95 = quantile(treatment, .95, type = 7, na.rm = T)
    ), by = "grade"]
  } else {
    stop("No valid reference!")
  }
  
  var = c("treatment", "qt05", "qt10", "qt15", "qt20", "qt25", "qt90", "qt95")
  attr_data = treatment_data[, c("aid", var), with=FALSE]
  edgelist = edgelist[, c("aid", "peer_aid")]
  
  if (include_self == "include_self"){
    edgelist_aid = unique(edgelist[,aid])
    edgelist = rbind(edgelist, data.table(aid = edgelist_aid, peer_aid=edgelist_aid))
  }
  
  edgelist = merge(x = edgelist, y = attr_data, by.x = c("peer_aid"), by.y = c("aid"), all.x = T)
  
  peer_treatment = edgelist[, .(
    peer_treatment_mean = mean(treatment, na.rm=T),
    peer_treatment_median = median(treatment, na.rm=T),
    peer_treatment_q05 = mean(treatment <= qt05, na.rm=T),
    peer_treatment_q10 = mean(treatment <= qt10, na.rm=T),
    peer_treatment_q15 = mean(treatment <= qt15, na.rm=T),
    peer_treatment_q20 = mean(treatment <= qt20, na.rm=T),
    peer_treatment_q25 = mean(treatment <= qt25, na.rm=T),
    peer_treatment_q90 = mean(treatment >= qt90, na.rm=T),
    peer_treatment_q95 = mean(treatment >= qt95, na.rm=T),
    N_peer = .N
  ), by = "aid"]
  
  return(peer_treatment)
}


measure_self_treatment_rank <- function(edgelist, treatment_data){
  
  edgelist = edgelist[, c('aid', 'peer_aid')]
  edgelist_aid = unique(edgelist[, "aid"])
  
  self_edge = data.table(edgelist_aid, edgelist_aid)
  names(self_edge) = c("aid", "peer_aid")
  edgelist = rbind(edgelist, self_edge)
  
  var = "treatment"
  attr_data = treatment_data[, c("aid", var), with = FALSE]
  
  edgelist = merge(x = edgelist, y = attr_data, by.x = c("peer_aid"), by.y = c("aid"), all.x = TRUE)
  edgelist = na.omit(edgelist)
  setorder(edgelist, aid, treatment)
  
  edgelist[, rank_treatment := frank(treatment, ties.method = "min"), by = "aid"]
  edgelist[, max_rank_treatment := max(rank_treatment), by = "aid"]
  edgelist[, prank_treatment := rank_treatment / max_rank_treatment]
  rank_treatment = edgelist[aid == peer_aid, c("aid", "rank_treatment", "prank_treatment")]
  
  return(rank_treatment)
}


## 4. combine and pull data
combine_data <- function(key, list_data_table) {
  for (i in 1:length(list_data_table)) {
    list_data_table[[i]] <- setkeyv(list_data_table[[i]], key)
  }
  
  merged_data = Reduce(function(...) merge(..., all.x = T), list_data_table)
  
  names(merged_data) = gsub(' ', '_', names(merged_data))
  names(merged_data) = gsub('\\.mean', '_peermean', names(merged_data))
  names(merged_data) = gsub('\\.', '_', names(merged_data))
  
  # final process before regression
  merged_data[, `:=`(
    female = female_1,
    white = as.integer(race_h_1 == 1),
    black = as.integer(race_h_1 == 2),
    hispanic = as.integer(race_h_1 == 3),
    other = as.integer(race_h_1 == 4),
    immig_1st = as.integer(immig_2_1 == 1),
    immig_2nd = as.integer(immig_2_1 == 2),
    immig_3rd = as.integer(immig_2_1 == 3),
    family_two = as.integer(famst5_1 == 1),
    family_step = as.integer(famst5_1 == 2),
    family_one = as.integer(famst5_1 == 6),
    family_other = as.integer(famst5_1 %in% c(7,8)),
    pa_educ = c_paeduc_max_1,
    pvt = pvt_1,
    sibsize = sibsize_1,
    assistance = assistance_1,
    parent_attachment = p_attachment_1,
    school_attachment = s_attachment_1,
    scid = scid_1
  )] # select what variables as control variables
  
  merged_data[female == 0, bfcesd_2 := ifelse(fcesd_2 > 22, 1, 0)]
  merged_data[female == 1, bfcesd_2 := ifelse(fcesd_2 > 24, 1, 0)]
  
  return(merged_data)
}


pull_data <- function(list_peer_type, peer_type_i, m_filter, m_reference, m_selfinclude){
  for (i in peer_type_i) {
    edgelist_input = list_peer_type[[i]]$edgelist
    peer_name = list_peer_type[[i]]$peer_name
    message(i, " | now processs: ", paste0("reg_", peer_name, "_", m_reference, "_", m_selfinclude))
    
    controls_peer = measure_peer_mean(edgelist_input, processed_data_inschool, peer_controls)
    
    treatment_peer = measure_peer_distribution(edgelist_input, treatment_data, include_self = m_selfinclude, filter = m_filter, reference = m_reference)
    
    self_treatment_rank = measure_self_treatment_rank(edgelist_input, treatment_data)
    
    # combine all data
    reg_data = combine_data(key = "aid", list(processed_data_wave1, processed_data_wave2, 
                                              weight_wave1, weight_wave2, 
                                              processed_data_inschool,
                                              treatment_data[treatment_data$aid != ""],
                                              treatment_peer, controls_peer, self_treatment_rank, network))
    
    export(reg_data, file.path(workdata_path, paste0("reg_", peer_name, "_", m_reference, "_",  m_selfinclude, ".dta")))
  }
}

