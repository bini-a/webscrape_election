################################################################################
# File:           helper.R
#
# Description:    a set of helper functions
#                 
# Created by:     Biniam
################################################################################



get_office_seat <- function(string) {
  result <- list()
  if (grepl("^SC", string)) {
    d <- gsub("^SC", "", string)
    result$prefix <- "SC"
    result$d <- as.numeric(d)
  } else if (grepl("^D", string)) {
    d <- gsub("^D", "", string)
    result$prefix <- "TC1"
    result$d <- as.numeric(d)
  } else {
    result$prefix <- NA
    result$d <- NA
  }
  return(result)
}

get_judge_or_justice_names <- function(string) {
  pat = "(?<=Justice |Judge ).*?(?= be retained in Office\\?)"
  
  match <- regmatches(string, regexpr(pat, string, perl = TRUE))
  if (length(match) == 0) {
    pattern <- "(?<=Justice |Judge ).*(?= be retained)"
    name <-
      regmatches(string, regexpr(pattern, string, perl = TRUE))
    return (name)
  }
  return(match)
}



extract_first <- function(name) {
  ##############################################################################
  # extract first name from candidate names
  ##############################################################################
  name_split <- strsplit(name, " ")
  first_name <- sapply(name_split, function(x)
    x[1])
  return (first_name)
}



extract_last <- function(name) {
  ##############################################################################
  # extract last name from candidate names
  ##############################################################################
  name_split <- strsplit(name, " ")
  last_name <- sapply(name_split, function(x)
    # this deals with empty name slots in your original list, returning NA
    if (length(x) == 0) {
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


check_candidate <- function(string) {
  split_string <- strsplit(string, " : ")[[1]]
  if (split_string[length(split_string)] == "Yes") {
    return(split_string[1])
  } else {
    return("*")
  }
}

get_party_dict <- function() {
  party_dict <- c(
    "republican" = "REP",
    "democrat" = "DEM",
    "independent" = "IND",
    "green" = "GRE",
    "libertarian" = "LIB",
    "non-partisan" = "NON",
    "other party" = "OTH",
    "unaffiliated" = "UNA",
    "unknown" = "UNK",
    "No Party Affiliation" = "UNA",
    "libertarian party of florida" = "LIB",
    "write-in" = "Write-In",
    "Independent Party" = "IND"
  )
  return (party_dict)
}