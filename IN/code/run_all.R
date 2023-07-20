
library(tidyr)
library(rvest)
library(dplyr)
library(pdftools)
library(stringi)
library(gsubfn)
library(tidyr)
library(fuzzyjoin)
library(stringdist)
library(readr)
library(RSelenium)
library(tidyverse)
library(netstat)
library(docstring)
library(data.table)
# Clean-up work space
rm(list = ls())

source("functions.R")
source("IN/code/use_browser.R")
source("IN/code/wrangle_data.R")

# clean folders if existing
clean_slate("IN/data/raw")  # local temp folder to store downloaded data using rselenium
clean_slate("IN/data/clean") # folder to save all cleaned data
clean_slate("IN/data/merged") # folder to save  merged cleaned data

run_all<-function(){
  # years to download
  list_years<- c("2020","2018", "2016", "2014", "2012", "2010",
                 "2008","2006", "2004", "2002")
  local_path<-paste(str_replace_all(getwd(), "/","\\\\"), "\\IN\\data\\raw", sep="")
  eCaps <- list(chromeOptions =
                list(prefs = list("profile.default_content_settings.popups" = 0L,
                                  "download.prompt_for_download" = FALSE,
                                  "directory_upgrade" = TRUE,
                                  # save downloaded files to temp raw folder
                                  "download.default_directory" = local_path,
                                  "safebrowsing.disable_download_protection" = TRUE)))
  # initialize rselenium
  rD <- rsDriver(browser="chrome", port=4444L, 
                 extraCapabilities = eCaps,
                 verbose=T)
  remDr <- rD[["client"]]
  
  link_year <- "https://indianavoters.in.gov/ENRHistorical/ElectionResults"

  # open browser and download all to raw folder
  use_browser(link_year, list_years, remDr)
  print("Finished  Downloading")
  # close server and finish
  remDr$close()
  if(rD[["server"]]$stop()){
    print("Closed browser")
  }

  # wrangle downloaded raw files
  clean_raw_folder()
  print("Cleaned files are saved in data/clean")

  # merge all clean_csv files into single office, election,
  # results and candidates files
  merge_years()
  print("Clean files are merged into data/merged folder")
}


run_all()
