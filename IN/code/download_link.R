################################################################################
# File:           link_download.R
#
# Description:    downloads files for the year 2020 and 2018, wrangles and
#                 cleans data and saves csv files in the folder data/clean_data
#
# Created by:     Biniam
# requires:
# provides:
################################################################################


source("IN/code/generate_csv_files.R")
source("IN/code/merge_office_candidate.R")
source("IN/code/make_election_csv.R")
source("IN/code/make_office_csv.R")
source("IN/code/make_results_csv.R")
source("IN/code/make_candidate_csv.R")

save_path <- "IN/data/clean/"
state <- "IN"
download_1 <- function(year_election, type_election, link_df) {
  office_candidate_list <-
    generate_csv_files(year_election, type_election, link_df)
  all_merged <-
    merge_office_candidate(office_candidate_list, type_election)
  election <-
    generate_election_csv(office_candidate_list, type_election)
  office <- generate_office_csv(all_merged)
  results <- generate_result_csv(all_merged)
  candidates <- generate_candidate_csv(all_merged)
  
  
  # save files
  print(
    paste(
      "Started saving files, Year:",
      year_election,
      "Election Type:",
      type_election
    )
  )
  write.csv(election,
            file = paste0(
              save_path,
              "election_",
              year_election,
              "_",
              type_election,
              ".csv"
            ))
  write.csv(office,
            file = paste0(
              save_path,
              "office_",
              year_election,
              "_",
              type_election,
              ".csv"
            ))
  write.csv(results,
            file = paste0(
              save_path,
              "results_",
              year_election,
              "_",
              type_election,
              ".csv"
            ))
  write.csv(
    candidates,
    file = paste0(
      save_path,
      "candidates_",
      year_election,
      "_",
      type_election,
      ".csv"
    )
  )
  print("Finished saving files")
}

download_2 <- function(year_election, type_election, link_df) {
  print("Start generating file")
  print(year_election)
  print(type_election)
  # extract link
  link_year <- link_df %>%
    filter(
      election_year == year_election &
        election_type == type_election & file_type == "Result"
    ) %>%
    select(link)
  # remove index.html from the end of link
  link_year <- gsub('.index.html', '', link_year)
  # download csv file from link
  download_csv_link <-
    paste(link_year, "/", "download/AllOfficeResults.csv", sep = "")
  df <- read.csv(download_csv_link)
  # colnames(df)<- c("Election","Jurisdiction.Name","Reporting.County.Name","DataEntry.Jurisdiction.Name",
  #                  "DataEntry.Level.Name","Office","Office.Category","Ballot.Order","Name.on.Ballot",
  #                  "Political.Party","Total.Votes")
  #
  ## select offices of interest
  office_list <- unique(df$Office.Category)
  office_interest <-
    c(
      "Judge, Circuit Court",
      "Judge, Superior Court",
      "Judge, Probate Court",
      "Prosecuting Attorney",
      "Clerk Of The Circuit Court" ,
      "County Sheriff",
      "Judge, Small Claims Court",
      "Judge, Town Court",
      "Attorney General",
      "Clerk of the Supreme Court",
      "Judge of the Circuit Court",
      "Judge of the Superior Court",
      "Clerk of the Circuit Court"
    )
  # filter offices
  df_office <- filter(df, Office.Category %in% office_interest)
  # multiple patterns for finding seat numbers
  pattern_list <- c("No.", "Ct.", "Court", "Seat")
  num_pattern <- " \\d{1}|"
  seat_extract <- paste(pattern_list, collapse = num_pattern)
  seat_extract <- paste(seat_extract, " \\d{1}", sep = "")
  df_office <-
    df_office %>% 
    mutate(office_type = ifelse(Office.Category=="Prosecuting Attorney", "DA",
          ifelse(Office.Category=="Attorney General", "AG",
          ifelse(Office.Category =="County Sheriff" ,"SF",
          ifelse(tolower(Office.Category) == tolower("Clerk Of The Circuit Court"), "CK2",
          ifelse(tolower(Office.Category) == tolower("Clerk Of The Supreme Court"), "CK1",
          ifelse(Office.Category=="Judge, Superior Court" , "TC1",
          ifelse(Office.Category =="Judge, Circuit Court", "TC2",
          ifelse(Office.Category=="Judge of the Superior Court" , "TC1",
          ifelse(Office.Category =="Judge of the Circuit Court", "TC2",
          ifelse(Office.Category =="Judge, Small Claims Court", "LC1",
          ifelse(Office.Category =="Judge, Town Court" , "LC2",
          ifelse(Office.Category =="Judge, Probate Court" , "LC3", NA)))))))))))),
          seat =str_extract(Office, seat_extract),
      num_courts = str_extract(Office,"(\\d{1,})\\w{1,}"))
  
  df_office <- df_office %>%
    mutate(
      num_courts = ifelse(is.na(str_extract(
        num_courts, "\\d{1,}"
      )), 1, str_extract(num_courts, "\\d{1,}")),
      seat = ifelse(is.na(str_extract(seat, "\\d{1,}")), 1, str_extract(seat, "\\d{1,}"))
    )
  df_office <- df_office %>%
    mutate(district = ifelse(
      is.na(num_courts),
      Jurisdiction.Name,
      paste(Jurisdiction.Name, num_courts, sep = "_")
    ))
  
  # assign date
  df_office$dates = NA
  if (type_election == "PRI") {
    df_office$dates <- c("2016-05-03")
  } else{
    df_office$dates <- c("2016-11-08")
  }
  
  # office id, election_id
  df_office <- df_office %>%
    mutate(
      office_id = paste(office_type, district, seat, sep = "_"),
      election_id = paste(state, dates, type_election, sep = "_"),
      candidate_name = Name.on.Ballot
    )
  
  # contested
  df_office <- df_office %>%
    group_by(Office) %>%
    mutate(contested = ifelse(n() > 1, 1, 0))
  
  is_special = as.integer(!type_election %in% c("GEN", "PRI"))
  runOff = 0
  # create elections csv
  election <-
    data.frame(NA,
               state,
               unique(df_office$dates),
               type_election,
               runOff,
               is_special)
  colnames(election) <-
    c("election_id", "state", "date", "type", "runoff", "special")
  # Create election_id column
  election <- election %>%
    mutate(election_id = ifelse(is.na(date), NA, paste(state, date, type, sep =
                                                         "_")))
  # create office.csv
  office <- generate_office_csv(df_office)
  # create result.csv
  results <- generate_result_csv_type2(df_office)
  # create candidates.csv
  candidates <- generate_candidate_csv(df_office)
  # save files
  print(paste(
    "Started saving files, Year:",
    year_election,
    "Election Type:",
    type_election
  ))
  write.csv(election,
            file = paste0(
              save_path,
              "election_",
              year_election,
              "_",
              type_election,
              ".csv"
            ))
  write.csv(office,
            file = paste0(save_path, "office_", year_election, "_", type_election, ".csv"))
  write.csv(results,
            file = paste0(
              save_path,
              "results_",
              year_election,
              "_",
              type_election,
              ".csv"
            ))
  write.csv(
    candidates,
    file = paste0(
      save_path,
      "candidates_",
      year_election,
      "_",
      type_election,
      ".csv"
    )
  )
  print("Finished saving files")
}