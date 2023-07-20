


########## Offices ##################

generate_offices<-function(all_merged){
  ##############################################################################
  # generate office.csv using merged data of offices and candidates
  # returns data-frame of offices
  ##############################################################################
  print("Generating office csv")
  offices_csv <- data.frame(all_merged)
  offices_csv <-
    offices_csv %>%
    select(election_id,
           office_id,
           district,
           Office,
           office_type,
           seat,
           contested) %>%
    mutate(
      n_winners = NA,
      selection_method = NA,
      retention_method = NA,
      open_seat = NA
    )
  colnames(offices_csv)[4] <- "office_local"
  colnames(offices_csv)[5] <- "office"
  print("Finished generating office csv")
  return (offices_csv)
}

########### Result  #######################
generate_results<-function(all_merged){
  # this is different due to grouping by office name and district and county
  # and there is no winner column found, different column names in
  # Party , votes
  print("Generating result csv")
  results <- all_merged %>%
    group_by(County, Office, District_orig) %>%
    # need to add county because of "statewide" districts
    summarise(vote_share = format(round(Votes / sum(Votes), digits = 2), 
                                  nsmall =2),
              across(),
              candidate_id = NA) %>%
    select(election_id,
           office_id,
           candidate_id,
           candidate_name,
           Party,
           Votes,
           vote_share)
  # drop unwanted columns for final office result
  results[, c(1, 2, 3)] <- list(NULL)
  # rename columns
  results <- results %>% rename(candidate = candidate_name,
                                party = Party,
                                votes = Votes)
  results <- results %>% mutate(
    party = recode(
      party,
      "Republican" = "REP",
      "Democratic" = "DEM",
      "Libertarian" = "LIB",
      "Independent" = "IND",
      "Non-partisan" = "NON",
      "Non Partisan" = "NON"
    )
  )
  print("Finished generating result csv")
  return (results)
}

extract_first<-function(name){
  ##############################################################################
  # extract first name from candidate names
  ##############################################################################
  name_split <- strsplit(name, " ")
  first_name <- sapply(name_split, function(x) x[1])
  return (first_name)
}
extract_last<-function(name){
  ##############################################################################
  # extract last name from candidate names
  ##############################################################################
  name_split <- strsplit(name, " ")
  last_name <- sapply(name_split, function(x)
    # this deals with empty name slots in your original list, returning NA
    if(length(x) == 0) {
      NA
      # now check for a suffix; if one is there, use the penultimate item
      # after stripping it of any punctuation
    } else if (x[length(x)] %in% c("Jr.", "Jr", "Sr.", "Sr")) {
      
      gsub("[[:punct:]]", "", x[length(x) - 1])
      
    } else {
      x[length(x)]
    })
  return (last_name)
}

########### Candidate #######################


generate_candidates<-function(all_merged){
  
  ##############################################################################
  # generates candidates.csv using merged data of offices and candidates
  # returns data-frame of candidates
  ##############################################################################
  print("Generating candidate csv")
  candidate_csv<-
    all_merged%>%
    mutate(candidate_id = NA, # will be fixed later
           first_name = extract_first(candidate_name),
           last_name=extract_last(candidate_name),
           prefix=NA,
           suffix=NA,
           gender=NA,
           race = NA,
           ethinicity=NA,
           age=NA,
           incumbent=NA)%>%
    select(election_id, 
           office_id,candidate_id, candidate_name, first_name, last_name, prefix, 
           suffix, gender, race, ethinicity, age, incumbent)
  
  # drop office names
  candidate_csv<- candidate_csv[,-1]
  # Select unique candidates based on election_id, office_id, candidate_name
  candidate_csv<- candidate_csv[!duplicated(
    candidate_csv[,c('election_id','office_id','candidate_name')]),]
  print("Finished generating candidate csv")
  return(candidate_csv)
}