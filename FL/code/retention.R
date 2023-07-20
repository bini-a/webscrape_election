
################################################################################
# File:           retention.R
#
# Description:    wrangle and munge retention elections
#                 
# Created by:     Biniam
################################################################################
source("FL/code/helper.R")


munge_retention <- function(df, retention_offices, state, date, type_election){
  df_3 <- data.frame()
  party_dict <- get_party_dict()
  is_special = as.integer(type_election == "SPE")
  
  for ( i in 1:length(retention_offices)) {
    office_i = df%>%
      filter(df$OfficeDesc == retention_offices[i])
    # retain judge "YES" or "NO"
    office_i = office_i%>%
      mutate(retain = CanNameLast,
             old_race_code = RaceCode)
    office_i$selection_method <- "RE"
    
    # SC1-SC5: supreme Court Judge, District 1-5
    # D11-D56: District Court Judge, District 11-56
    
    office_i$FullName<- sapply(office_i$OfficeDesc, function(x) get_judge_or_justice_names(x))
    office_i =  office_i%>%
      mutate(CanNameFirst = extract_first(FullName),
             CanNameLast=extract_last(FullName))
    
    office_i$RaceCode <- sapply(office_i$RaceCode, function(x) get_office_seat(x)$prefix)
    
    office_i$district <- ifelse(grepl("^SC", office_i$RaceCode), substr(office_i$RaceCode, 3, nchar(office_i$RaceCode)), NA)
    
    if (!is.na(office_i[1,]$RaceCode) && office_i[1,]$RaceCode == "SC"){
      office_i$district <-  substr(office_i$old_race_code, 3, nchar(office_i$old_race_code))
      office_i$seat<- "000"
      
    }else{
      office_i$district <-  substr(office_i$old_race_code,2,2)
      office_i$seat<-   substr(office_i$old_race_code,3,3)
    }
    office_i<- office_i%>%mutate(seat = ifelse(is.na(seat) | seat == "*", "000",seat ))
    office_local = NA
    office_i_election_id = paste(state, date, type_election, sep = "_")
    
    office_i= office_i%>%
      mutate(election_id = office_i_election_id,
             state = state,
             date = date, 
             type = type_election,
             runoff = 0, # place holder for runoff, changed next lines
             special = is_special,
             office = RaceCode,
             office_local=NA)
    
    df_3 <- bind_rows(df_3, office_i)
    
  }
  
  # clean seat and district, create office_id and candidate column
  df_3 <- df_3 %>%
    mutate(seat = ifelse(is.na(seat), "000", seat),
           district = ifelse(is.na(district), state, district),
           office_id = paste(office, district, seat, sep="_"),
           candidate = paste0(CanNameFirst, " ", CanNameLast," : " ,retain))
  

  results_summary_2 <- df_3 %>% 
    select("election_id","office_id", "candidate", "PartyName", "state",
           "date", "type", "runoff", "special", "office", "CanVotes", "district", "seat")%>%
    group_by(election_id, office_id, candidate) %>%
    summarize(total_votes = sum(CanVotes), .groups = NULL)%>%
    mutate(vote_share = 1.0 * total_votes / sum(total_votes)) %>%
    group_by(election_id, office_id) %>%
    mutate(rank = dense_rank(desc(vote_share))) %>%
    mutate(majority_winner = ifelse(any(vote_share > 0.5), 1, 0)) %>%
    mutate(winner = ifelse(majority_winner == 1, ifelse(vote_share == max(vote_share), 1, 0),
                           ifelse(rank <= 2, 1, 0))) %>%
    mutate(n_winners = ifelse(majority_winner==1,1,2))%>%
    ungroup()

  
  merged_results_2 <- left_join(df_3, results_summary_2, by = c("election_id", "office_id", "candidate"))
  # contested=0 for retention
  
  
  merged_results_2 <- merged_results_2%>%
    mutate(contested = 0,
           runoff = 0,
           n_winners = 1,
           party = case_when(
             tolower(PartyName) %in% tolower(names(party_dict)) ~ party_dict[tolower(PartyName)],
             TRUE ~ as.character("OTH")
           ))

  
  # Apply function to the candidate column of df
  results_summary_2$candidate <- sapply(results_summary_2$candidate, check_candidate)
  results_summary_2 <- results_summary_2[results_summary_2$candidate != "*", ]
  
  merged_results_2 <-merged_results_2[merged_results_2$retain == "Yes", ]
  merged_results_2$candidate <- merged_results_2$FullName
  
  
  # create results_csv
  results_csv_2 <-  results_summary_2%>%
    select(election_id,  office_id, candidate, total_votes, vote_share, winner)

  
  party_from_merged<- merged_results_2%>%
    select("election_id", "office_id", "candidate", "party")%>%
    distinct()
  
  parties<- results_csv_2%>%
    right_join(party_from_merged, by = c("election_id", "office_id", "candidate"))
  
  results_csv_2$party = parties$party
  results_csv_2 <-results_csv_2%>%
    mutate(votes= total_votes,
           vote_share = round(vote_share,3),
           candidate_id = NA)%>%
    select(election_id, office_id, candidate_id,  candidate, party, votes, vote_share, winner)
  
  elections_csv_2 <- merged_results_2%>%
    select(c("election_id", "state", "date", "type"))%>%
    distinct()
  
  offices_csv_2 <- merged_results_2 %>%
    mutate(retention_method = NA,
           open_seat = NA)%>%
    select(
      election_id,
      office_id,
      district,
      office_local,
      office,
      seat,
      n_winners,
      selection_method,
      retention_method,
      contested,
      open_seat,
      runoff,
      special
    ) %>%
    distinct()
  
  
  
  ret = list("election" = elections_csv_2,
             "result" = results_csv_2,
             "office" = offices_csv_2)
  return (ret)
  
}

