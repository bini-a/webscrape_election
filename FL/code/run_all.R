################################################################################
# File:           run_all.R
# Created by:     Biniam
################################################################################


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

source("aj_functions.R")
# clean folders if existing
clean_slate("FL/data/raw")  # local temp folder to store downloaded data using rselenium
clean_slate("FL/data/clean") # folder to save all cleaned data
clean_slate("FL/data/merged") # folder to save  merged cleaned data

source("FL/code/use_browser.R")
source("FL/code/wrangle_data.R")
source("FL/code/merge_cleaned.R")

run_all <- function(){
  
  # download all csv from link using Rselenium
  use_browser()
  
  # wrangle_data
  wrangle_data()
  
  # #merge all cleaned data from all years
  merge_cleaned()
  
}

run_all()



