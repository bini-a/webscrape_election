


################################################################################
# File:           wrangle_data.R
#
# Description:    wrangle data from raw folder,
#                 save cleaned data to clean_csv folder
# Created by:     Biniam
################################################################################

source("FL/code/non_retention.R")
source("FL/code/retention.R")
source("FL/code/helper.R")

state = "FL"
save_path <- "FL/data/clean/"
raw_folder <- "FL/data/raw"
merge_folder <- "FL/data/merged/"




wrangle_data <- function() {
  ########################################################
  ## wrangle data from raw folder
  ## save cleaned data to clean_csv folder
  ######################################################
  file_list <- list.files(raw_folder)
  for (file_name in file_list) {
    file_path <- paste0(raw_folder, "\\", file_name)
    df <- read.csv(file_path, header = TRUE, sep = "\t")
    # filter by desired office categories
    split_name <- strsplit(file_name, "_")[[1]]
    date <- split_name[4]
    date <- gsub(".csv", "", date)
    type_election <- split_name[3]
    year_election <- split_name[2]
    print("---- Start  ----- ")
    print(paste(date, type_election, year_election))
    all_avail_offices <- unique(df$OfficeDesc)
    all_offices_interest <-
      c("Attorney General",
        "Circuit Judge",
        "State Attorney",
        "Public Defender")
    df <-
      df[df$OfficeDesc %in% all_offices_interest |
           grepl("retained in office", df$OfficeDesc, ignore.case = TRUE),]
    if (nrow(df) == 0) {
      print("No judicial offices found")
      next
    }
    is_special = as.integer(type_election == "SPE")
    list_offices = unique(df$OfficeDesc)
    # Identify indices of retention rows
    idx <- grep("retained in office", list_offices, ignore.case = TRUE)
    if (length(idx) == 0) {
      non_retention_offices <- list_offices
      data_1 = munge_non_retention(df, non_retention_offices, state, date, type_election)
      combine_offices <- data_1$office
      combine_elections <- data_1$election
      combine_results <-  data_1$result
      
      
    } else {
      retention_offices <- list_offices[idx]
      non_retention_offices <- list_offices[-idx]
      data_1 = suppressWarnings(munge_non_retention(df, non_retention_offices, state, date, type_election))
      data_2 = suppressWarnings(munge_retention(df, retention_offices, state, date, type_election))
      
      print("Merging retention and non-retention")
      
      combine_offices <- unique(rbind(data_1$office, data_2$office))
      combine_elections <-
        unique(rbind(data_1$election, data_2$election))
      combine_results <-  unique(rbind(data_1$result, data_2$result))
    }
    # save results
    write.csv(
      combine_results,
      file = paste0(
        save_path,
        "result_",
        year_election,
        "_",
        type_election,
        ".csv"
      ),
      row.names = FALSE
    )
    
    
    write.csv(
      combine_elections,
      file = paste0(
        save_path,
        "election_",
        year_election,
        "_",
        type_election,
        ".csv"
      ),
      row.names = FALSE
    )
    
    write.csv(
      combine_offices,
      file = paste0(
        save_path,
        "office_",
        year_election,
        "_",
        type_election,
        ".csv"
      ),
      row.names = FALSE
    )
    print("----   end ----- ")
    
  }
  
}
