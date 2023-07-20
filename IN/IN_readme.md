# garrett_elections

Local judicial and criminal justice elections project.

# Automated Web Browsing in R with `RSelenium`

## Prerequisites

-   Download and install latest [Java SE Development Kit](https://www.oracle.com/java/technologies/downloads/#jdk17-mac)

["box link"](https://duke.app.box.com/folder/144114646971)

## Working on Indiana

["Indiana Link"](https://www.in.gov/sos/elections/election-commission/election-results/)

## Data Format from Indiana Link

-   The data formats are varied for the different years.
-   Format 1: (2018-2020) Have two components candidates info and office info.
-   Format 2 : 2016 Have office info
-   Format 3:(2014-2002) Different link - Use Rselenium to download files
-   The candidates info contains the date of election, candidate name, party, office name
-   The office info contains District/County, Office Name, Office Category, Candidate Name, Political Party, Winner, No of Seats and Total Votes.
-   However, not all candidates exist in both candidates and office info data; the merging of these data is done based on names that exist in both

## Notes

-   District is used as county_name + number_courts
-   Non contested elected officials are given 100% vote share
-   Statewide district and missing district are assigned\<- seat=1
-   Candidates who run statewide have different entries for all each county they run for. So a candidate may have multiple office_id's.
-   Missing court numbers are assigned num_courts=1
-   For the numbers unable to extract and all other categories, 1 is used as the number of courts.
-   Non contested elected officials are given 100% vote share (0 votes have NA vote share)
