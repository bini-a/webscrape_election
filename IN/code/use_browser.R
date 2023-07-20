################################################################################
# File:           id_download.R
#
# Description:   Opens up browser, download csv files for the list_years, renames
#                files accordingly and saves in the folder data/raw_data
# Created by:     Biniam
# requires:       
# provides:   
################################################################################

raw_folder<- "\\IN\\data\\raw"
local_path<-paste(str_replace_all(getwd(), "/","\\\\"), raw_folder, sep="")


use_browser<-function(link_year ,list_years, remDr){
  #############################################################
  # Opens up browser, download csv files for the list_years, renames
  # files accordingly and saves in the folder data/raw_data
  ############################################################
  
  # Navigate to the link
  remDr$navigate(link_year)
  Sys.sleep(1) # give the binary a moment
  
  for (year_election in list_years){
    for(type_election in c("GEN", "PRI")){
      print("Generating csv files")
      print(year_election)
      print(type_election)
      print("---------------")

      # find all year list from a drop down menu
      year_drop_down<-remDr$findElement(using="xpath", "//*[(@id = 'drpElectionYears')]")
      year_list<-as.list(str_split(year_drop_down$getElementText(), "\n")[[1]])
      
      option_index =1
      while(option_index<=length(year_list)){
        if(year_list[option_index]==year_election){
          break
        }
        option_index = option_index+1
      }
      # Create year indexing to the options in the year drop_down menu
      option_index<-paste0("/option[", option_index, "]")
      xpath_year_option<- paste0("//*[(@id = 'drpElectionYears')]", option_index)
      
      # select the appropriate year
      remDr$findElement(using="xpath", xpath_year_option)$clickElement()
      # apply year filter
      remDr$findElement(using="xpath", "//*[(@id = 'btnApplyFilter')]")$clickElement()
      
      Sys.sleep(1)
      # Select type of election
      if (type_election =="PRI"){
        # click on primary tab
        remDr$findElement(using="xpath","//*[(@id = 'atabPrimary')]")$clickElement()
      }
      else if(type_election=="GEN"){
        # click on general tab
        remDr$findElement(using="xpath","//*[(@id = 'atabGeneral')]")$clickElement()
      }
      Sys.sleep(1)
      # extract election date from the current link
      date_text<- remDr$findElement(using="xpath","//*[(@id = 'spanElectionType')]")$getElementText()
      date_text<-str_replace_all(date_text,"INDIANA |Primary Election|General Election","")
      date<-as.Date(date_text, format='%B %d, %Y')
      print(date)
    
      Sys.sleep(1)
      
      # Find data filtering button and click
      remDr$findElement(using="xpath",value = "//*[(@id = 'imgElectionDataGridFilter')]")$clickElement()
      # Select all available counties
      select_obj<- remDr$findElement(using = 'xpath', value = "//*[@id='county']")
      sel_obj<- select_obj$selectTag()
      list_values<- sel_obj$value
      xpath_val <- "//*/option[@value ='"
    
      # click the first option to highlight choice
      curr_xpath_val<- paste(xpath_val,list_values[1],"']",sep="")
      remDr$findElement(using = 'xpath', value = curr_xpath_val)$clickElement()
      select_obj$sendKeysToElement(list("A"))
      # select all using ctrl+ a
      select_obj$sendKeysToElement(list( key="control","a"))
      # un-select the "ALL counties option"
      remDr$findElement(using = 'xpath', value = curr_xpath_val)$clickElement()
      
      # Click apply filter button
      click_apply<- remDr$findElement(using="xpath",
        value = "//*[@id='filter-tab1Desc']/div[3]/button[1]")$clickElement()
      
      # Find and click the download csv button
      remDr$findElement(using="xpath","//*[(@id = 'imgElectionDataGridCsv')]")$clickElement()
      
      # Give a break to download file
      Sys.sleep(3)
      # 
      # Rename the downloaded file accordingly
      tmpshot <- fileSnapshot(local_path)
      curr_file <- rownames(tmpshot$info[which.max(tmpshot$info$mtime),])
      # check if new file is not downloaded
      if(grepl("raw",curr_file)){
        print("File downloading error, previous downloads are found")
        next
      }
      curr_file_path<- paste0(local_path,"\\", curr_file)
      renamed_file<- paste0("raw_",year_election,"_",type_election,"_",date, ".csv")
      renamed_file_path<- paste0(local_path,"\\",renamed_file )
      file.rename(curr_file_path, renamed_file_path)
      print(paste("Renamed from", curr_file, "to", renamed_file))
    }
  }
  }
