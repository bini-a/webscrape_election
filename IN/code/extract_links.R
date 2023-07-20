################################################################################
# File:           extract_links.R
#
# Description:    extract all needed links for indian election and returns 
#                 a data frame of link,election_year, election_type, file_type
#
# Created by:     Biniam
# requires:       
# provides:       link_df
################################################################################

extract_links<-function(){
  ##################################################################################
  # Extract general, primary and special election links from the indiana gov website
  # election_type: GEN, PRI, SPE 
  # file_type: result of election for office, candidates info
  # returns data-frame(link, year, election_type, file_type)
  ####################################################################################
  print("extracting links")
  
  indiana_election_link = "https://www.in.gov/sos/elections/election-commission/election-results/"
  indiana_election <- read_html(indiana_election_link)
  
  
  link_list <- indiana_election%>% html_nodes("#subpage-text-container a")
  year_list<- c()
  election_type<- c()
  file_type<-c() # candidates, result
  selected_link_list<-c()
  for (link in link_list ){
    # print(link)
    link_text <- link%>%html_text()
    year<- sub('.*(\\d{4}).*', '\\1', link_text)
    # select years upto 2011
    if (year>= 2011){
      if (grepl("GENERAL ELECTION", link_text, ignore.case=TRUE)){
        type<-"GEN"
        if (grepl("Candidate", link_text, ignore.case=TRUE)){
          file<-"Candidates"
        }
        else{
          file<-"Result"
        }
        selected_link_list<-c(selected_link_list,link%>%html_attr("href"))
        year_list<-c(year_list, year)
        election_type<-c(election_type, type)
        file_type<-c(file_type, file)
      }
      else if (grepl("PRIMARY ELECTION", link_text, ignore.case=TRUE)){
        type<-"PRI"
        if (grepl("Candidate", link_text, ignore.case=TRUE)){
          file<-"Candidates"
        }
        else{
          file<-"Result"
        }
        selected_link_list<-c(selected_link_list,link%>%html_attr("href"))
        year_list<-c(year_list, year)
        election_type<-c(election_type, type)
        file_type<-c(file_type, file)
      }
      else if (grepl("SPECIAL ELECTION", link_text, ignore.case=TRUE)){
        type<-"SPE"
        file<-""
        
        selected_link_list<-c(selected_link_list,link%>%html_attr("href"))
        year_list<-c(year_list, year)
        election_type<-c(election_type, type)
        file_type<-c(file_type, file)
      }
    }
  }
  selected_link_list<-
    case_when(
      !str_detect(selected_link_list,"http") ~ paste("https://www.in.gov", selected_link_list,sep=""),
      TRUE ~ selected_link_list
    )
  link_df<-data.frame(selected_link_list, year_list, election_type, file_type)
  # delete duplicate 2016 General supplemental info
  link_df<-link_df[-c(12),]
  colnames(link_df)<-c("link", "election_year", "election_type", "file_type")
  # View(link_df)
  # edit manually link for 2020 
  "https://enr.indianavoters.in.gov/archive/2020General/index.html" -> link_df[link_df$election_year == 2020 & 
                                                                                 link_df$election_type=="GEN" & 
                                                                                 link_df$file_type =="Result",]$link
  
  print("Finish extracting links")
  return (link_df)
}
