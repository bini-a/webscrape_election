
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

# local download path for files
local_path<-paste(str_replace_all(getwd(), "/","\\\\"), "\\WA\\data\\raw", sep="")
eCaps <- list(chromeOptions =
                list(prefs = list("profile.default_content_settings.popups" = 0L,
                                  "download.prompt_for_download" = FALSE,
                                  "directory_upgrade" = TRUE,
                                  # save downloaded files to temp raw folder
                                  "download.default_directory" = local_path,
                                  "safebrowsing.disable_download_protection" = TRUE)))

rD <- rsDriver(browser = "chrome",
               chromever = "103.0.5060.53",
               port = free_port(),
               extraCapabilities = eCaps,
               verbose=FALSE,
               check = TRUE)
remDr <- rD[["client"]]
# link to election data by year
link_df<-read.csv("WA/data/raw_data/links.csv")

for(year in c("2021","2017","2013","2012", "2009", "2008", "2007")){
# for(year in c( "2007")){
    
  year_link<-link_df%>%filter(Year==year)
  data_link<- year_link$link[1]
  remDr$navigate(paste0(data_link, "Export.html"))
  Sys.sleep(2)
  if(year>="2014"){
    dw_link<- remDr$findElement(using="xpath", value = "//*[@id='ResultsContent']/div/table[1]/tbody/tr[3]/td[2]/a")
    }
  Sys.sleep(2)
  # //*[@id="ResultsContent"]/table[1]/tbody/tr[3]/td[2]/a
  #ResultsContent > table:nth-child(7) > tbody > tr:nth-child(3) > td:nth-child(2) > a
  #ResultsContent > table:nth-child(6) > tbody > tr:nth-child(3) > td:nth-child(2) > a
# // 2015
#   }
# 
# # select year
# remDr$findElement(using="xpath", value = "//*[@id='ResultsContent']/div/table[1]/tbody/tr[3]/td[2]/a")$clickElement()
# # close server and finish
remDr$close()
# if(rD[["server"]]$stop()){
#   print("Closed browser")
# }

# remDr$findElement(using="xpath", value = "//*[@id='ResultsContent']/table[1]/tbody/tr[3]/td[2]/a")$clickElement()
