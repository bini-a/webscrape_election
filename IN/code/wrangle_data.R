
source("IN/code/generate_csv_files.R")

state = "IN"
save_path<- "IN/data/clean/"
raw_folder<- "IN/data/raw"
merge_folder<-"IN/data/merged/"
clean_raw_folder<-function(){
  ########################################################
  ## wrangle data from raw folder
  ## save cleaned data to clean_csv folder
  ######################################################
  file_list<-list.files(raw_folder)
  for (file_name in file_list){
    file_path<- paste0(raw_folder,"\\",file_name)
    df<- read.csv(file_path)
    # filter by desired office categories
    split_name<-strsplit(file_name,"_")[[1]]
    date<- split_name[4]
    date<- gsub(".csv","", date)
    type_election <-split_name[3]
    year_election<- split_name[2]
    office_list <- unique(df$Office)
    office_interest <-
      c("Judge, Circuit Court",
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
    # strip spaces in office column
    df$Office <- trimws(df$Office, which = c("both"))
    # filter offices
    df <-  df[df$Office %in% office_interest, ]
    # rename District column to District_orig
    df<- df%>% rename(District_orig = District)
    # drop rows with missing district name
    df<- df[!(df$District_orig==""),]
    ## Assign office category
    print("Assigning office")
    df<-df%>%
      mutate(office_type = 
               ifelse(Office=="Prosecuting Attorney", "DA",
              ifelse(Office=="Attorney General", "AG",
              ifelse(Office =="County Sheriff" ,"SF",
              ifelse(tolower(Office) == tolower("Clerk Of The Circuit Court"),
                     "CK2",
              ifelse(tolower(Office) == tolower("Clerk Of The Supreme Court"), 
                     "CK1",
              ifelse(Office=="Judge, Superior Court" , "TC1",
              ifelse(Office =="Judge, Circuit Court", "TC2",
              ifelse(Office=="Judge of the Superior Court" , "TC1",
              ifelse(Office =="Judge of the Circuit Court", "TC2",
              ifelse(Office =="Judge, Small Claims Court", "LC1",
              ifelse(Office =="Judge, Town Court" , "LC2",
              ifelse(Office =="Judge, Probate Court" , 
                     "LC3", NA)))))))))))),
             seat = ifelse(District_orig=="Statewide",NA, 
                           str_extract(District_orig, "No. \\d{1,}$")),
             num_courts = ifelse(District_orig=="Statewide",NA, 
                                 str_extract(District_orig,
                                             "(\\d{1,})\\w{1,}")))
    
    # Additional multiple patterns for finding seat numbers
    pattern_list <- c("No.", "Ct.", "Court", "Seat")
    num_pattern <- " \\d{1}|"
    seat_extract <- paste(pattern_list, collapse = num_pattern)
    seat_extract <- paste(seat_extract, " \\d{1}", sep = "")
    
    
    df <- df %>%
      mutate(
        seat = ifelse(is.na(seat), str_extract(District_orig, seat_extract), seat),
        num_courts = ifelse(
          is.na(num_courts),
          str_extract(District_orig, "(\\d{1,})\\w{1,}"),
          num_courts
        )
      )
    
    # clean up seat and num_courts column by filtering only numerical values
    df <- df %>%
      mutate(
        num_courts = ifelse(is.na(str_extract(
          num_courts, "\\d{1,}"
        )), 1, str_extract(num_courts, "\\d{1,}")),
        seat = ifelse(is.na(str_extract(seat, "\\d{1,}")), 1,
                      str_extract(seat, "\\d{1,}"))
      )
    # assign new district column
    
    # District : County_name + number_courts
    df <- df %>%
      mutate(district = ifelse(is.na(num_courts), County, 
                               paste(County, num_courts, sep ="_")))
    
    # assign date column
    df$dates = date
    
    # office id, election_id
    # Office_id : office_district_seat
    
    df <- df %>%
      mutate(
        office_id = paste(office_type, district, seat, sep = "_"),
        election_id = paste(state, dates, type_election, sep = "_"),
        candidate_name = Candidate
      )
    # contested
    #group by office name and District
    #contested=1 if there is more than one office and district combinations
    df <- df %>%
      group_by(Office, District_orig, County) %>%
      mutate(contested = ifelse(n() > 1, 1, 0))
    is_special = as.integer(!type_election %in% c("GEN", "PRI"))
    runOff = 0
    
    # create elections csv
    election <- data.frame(NA, state, unique(df$dates), type_election,
                           runOff, is_special)
    colnames(election) <-
      c("election_id", "state", "date", "type", "runoff", "special")
    # Create election_id column
    election <- election %>%
      mutate(election_id = ifelse(is.na(date), NA, paste(state, date, type,
                                                         sep ="_")))
    
    office <- generate_offices(df)
    
    # create result.csv
    results <- generate_results(df)
    
    # create candidates.csv
    candidates <- generate_candidates(df)
    
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
              file = paste0(save_path, "office_", year_election, "_",
                            type_election, ".csv"))
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
  print("--------------------")
  print("Finsihed cleaning raw data folder")
}

merge_years <- function() {
  ##########################################################
  ### Merge all clean csv files into single office, election,
  ## candidate and result files
  ##########################################################
  list_files <- list.files(save_path)
  election <-
    paste0(save_path, list_files[startsWith(list_files, "e")])
  result <- paste0(save_path, list_files[startsWith(list_files, "r")])
  office <- paste0(save_path, list_files[startsWith(list_files, "o")])
  candidates <-
    paste0(save_path, list_files[startsWith(list_files, "c")])
  
  total_election <- lapply(election, read.csv)
  total_election <- bind_rows(total_election)
  total_election <- total_election %>%
    select("election_id", "state", "date", "type", "runoff", "special")
  write.csv(total_election, file = paste0(merge_folder, "election_all.csv"))
  rm(total_election)
  
  total_result <- lapply(result, read.csv)
  total_result <- bind_rows(total_result)
  total_result <- total_result %>%
    select(
      "election_id",
      "office_id",
      "candidate_id",
      "candidate",
      "party",
      "votes",
      "vote_share"
    )
  write.csv(total_result, file = paste0(merge_folder, "result_all.csv"))
  rm(total_result)
  
  total_office <- lapply(office, read.csv)
  total_office <- bind_rows(total_office)
  total_office <- total_office %>% select(
    "election_id",
    "office_id",
    "district",
    "office_local",
    "office",
    "seat",
    "contested",
    "n_winners",
    "selection_method",
    "retention_method",
    "open_seat"
  )
  write.csv(total_office, file = paste0(merge_folder, "office_all.csv"))
  rm(total_office)
  
  total_candidates <- lapply(candidates, read.csv)
  total_candidates <- bind_rows(total_candidates)
  total_candidates <- total_candidates %>%
    select(
      election_id,
      office_id,
      candidate_id,
      candidate_name,
      first_name,
      last_name,
      prefix,
      suffix,
      gender,
      race,
      ethinicity,
      age,
      incumbent
    )
  write.csv(total_candidates,
            file = paste0(merge_folder, "candidates_all.csv"))
  rm(total_candidates)
  
}
