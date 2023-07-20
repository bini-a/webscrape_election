################################################################################
# File:           non_retention.R
#
# Description:    wrangle and munge non-retention elections
#                 
# Created by:     Biniam
################################################################################


source("FL/code/helper.R")
munge_non_retention <- function(df, non_retention_offices, state, date, type_election){
  df_2 <- data.frame()
  
  party_dict <- get_party_dict()
  is_special = as.integer(type_election == "SPE")
  for (i in 1:length(non_retention_offices)) {
    office_i = df %>%
      filter(df$OfficeDesc == non_retention_offices[i])
    office_i$office_i_type = NA
    office_i$selection_method = NA
    # check type, count number of parties and determine election type
    num_parties <- length(unique(office_i$PartyName))
    office_i_type <-  0
    if (type_election == "GEN") {
      # If Gen, selection: non-partisan
      office_i$office_i_type = "GEN"
      office_i$selection_method = "NPE"
    } else{
      # If NOP->PRI
      if (num_parties == 1 &
          unique(office_i$PartyName)[1] == "Non-Partisan") {
        # If Pri , selection: non-partisan
        office_i$office_i_type = "PRI"
        office_i$selection_method = "NPE"
      } else{
        # else PRI-R, PRI-D selection: partisan
        for (party_type in unique(office_i$PartyName)) {
          val <- ifelse(party_type == "Democrat", "PRI-D", "PRI-R")
          office_i$office_i_type[office_i$PartyName == party_type] <-val
          office_i$selection_method[office_i$PartyName == party_type] <-"PE"
        }
      }
    }
    office_i <- office_i %>%
      mutate(election_id = paste(state, date, office_i_type, sep = "_"))
        # assign election_id
    office_i = office_i %>%
      mutate(
        state = state,
        date = date,
        type = office_i_type,
        runoff = 0,
        # place holder for runoff, changed next lines
        special = is_special
      )
    df_2 <- bind_rows(df_2, office_i)
  }
  
  # encode office codes, assign district and seat
  df_2 <- df_2 %>%
    mutate(
      office = case_when(
        OfficeDesc == "Attorney General" ~ "AG",
        OfficeDesc == "Circuit Judge" ~ "TC2",
        OfficeDesc == "State Attorney" ~ "SA",
        OfficeDesc == "Public Defender" ~ "PD",
        TRUE ~ as.character(OfficeDesc)
      ),
      district = Juris1num,
      seat = Juris2num,
      office_local = OfficeDesc
    )
  # clean seat and district, create office_id and candidate column
  df_2 <- df_2 %>%
    mutate(
      seat = ifelse(is.na(seat), "000", seat),
      district = ifelse(is.na(district), state, district),
      office_id = paste(office, district, seat, sep = "_"),
      candidate = paste(CanNameFirst, CanNameLast)
    )
  
  # create summary of election results
  # if majority winner is found, assign the candidate with max vote as winner,
  # else assign the top_two as winners
  
  results_summary <- df_2 %>%
    select(
      "election_id",
      "office_id",
      "candidate",
      "PartyName",
      "state",
      "date",
      "type",
      "runoff",
      "special",
      "office",
      "CanVotes",
      "district",
      "seat"
    ) %>%
    group_by(election_id, office_id, candidate) %>%
    summarize(total_votes = sum(CanVotes)) %>%
    mutate(vote_share = total_votes / sum(total_votes)) %>%
    group_by(election_id, office_id) %>%
    mutate(rank = dense_rank(desc(vote_share))) %>%
    mutate(majority_winner = ifelse(any(vote_share > 0.5), 1, 0)) %>%
    mutate(winner = ifelse(
      majority_winner == 1,
      ifelse(vote_share == max(vote_share), 1, 0),
      ifelse(rank <= 2, 1, 0)
    )) %>%
    mutate(n_winners = ifelse(majority_winner == 1, 1, 2)) %>%
    ungroup()
  
  
  num_contestants <- results_summary %>%
    group_by(election_id, office_id) %>%
    summarise(num_candidates = n_distinct(candidate)) %>%
    mutate(contested = ifelse(num_candidates >= 2, 1, 0))
  
  results_summary <-
    left_join(results_summary,
              num_contestants,
              by = c("election_id", "office_id"))
  
  # merge results_summary with df_2
  merged_results <-
    left_join(df_2,
              results_summary,
              by = c("election_id", "office_id", "candidate"))
  # ASSUME runoff does not happen in pri-d or pri-r
  # for rows with n_winners ==2 and election type is not PRI-D or PRI-R, make them run-off
  merged_results <- merged_results %>%
    mutate(runoff = if_else(
      type %in% c("PRI-D", "PRI-G"),
      runoff,
      if_else(n_winners == 2, 1, 0)
    )) %>%
    mutate(n_winners = if_else(runoff == 0, 1, n_winners))
  
  
  merged_results <- merged_results %>%
    mutate(party = case_when(
      tolower(PartyName) %in% tolower(names(party_dict)) ~ party_dict[tolower(PartyName)],
      TRUE ~ as.character("OTH")
    ))
  
  if (type_election == "RUNOFF") {
    merged_results$runoff <- 1
  }
  # create results_csv
  results_csv <-  results_summary %>%
    select(election_id,
           office_id,
           candidate,
           total_votes,
           vote_share,
           winner)
  
  # get party name from merged df
  
  match_index <-
    match(
      paste(
        results_csv$election_id,
        results_csv$office_id,
        results_csv$candidate
      ),
      paste(
        merged_results$election_id,
        merged_results$office_id,
        merged_results$candidate
      )
    )
  results_csv$party <- merged_results$party[match_index]
  results_csv$selection_method <-
    merged_results$selection_method[match_index]
  
  results_csv <- results_csv %>%
    mutate(
      votes = total_votes,
      vote_share = round(vote_share, 3),
      candidate_id = NA
    ) %>%
    select(election_id,
           office_id,
           candidate_id,
           candidate,
           party,
           votes,
           vote_share,
           winner)
  
  
  
  elections_csv <- merged_results %>%
    select(c("election_id", "state", "date", "type")) %>%
    distinct()
  
  
  
  offices_csv <- merged_results %>%
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
    open_seat,
    contested,
    runoff,
    special
    ) %>%
    distinct() 
  
  ret = list("election" = elections_csv,
             "result" = results_csv,
             "office" = offices_csv)
  return (ret)
  
}
