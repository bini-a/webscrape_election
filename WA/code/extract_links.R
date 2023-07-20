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


data_link<-"https://www.sos.wa.gov/elections/research/election-results-and-voters-pamphlets.aspx"

read_data<-read_html(data_link)

tables<-read_data%>%html_nodes("table")

# skip first table "archived results"
# skip second table "year 2022" - since it is not finalized

data_tables<-tables[3:length(tables)]
year_data<-data.frame()

for(table in data_tables){
  year_data<-rbind(year_data, data.frame(table%>%html_table())[,c("X1","X2")])
}
colnames(year_data)<-c("election_type", "Date")
# remove presidential elections
year_data<-year_data[- grep("Presidential", year_data$election_type),]
# standardize date format
year_data$Date<-as.Date(year_data$Date,format = "%m/%d/%Y")
year_data$Year<- substr(year_data$Date,1,4)
# extract links for each input
base_link<-"https://results.vote.wa.gov/results/"
year_data$link<-base_link
year_data$link<-paste0(base_link, str_replace_all(year_data$Date, c("-"="")), "/")
year_data<-year_data[,c("Year", "election_type", "link", "Date")]
write.csv(year_data, "WA/data/raw_data/links.csv")
