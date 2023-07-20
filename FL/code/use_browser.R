

################################################################################
# File:           use_browser.R
#
# Description:   Opens up browser, download all files for the listed_years, renames
#                files accordingly and saves in the folder data/raw_data
# Created by:    Biniam 
# requires:      Specify your  browser for initializing Rselenium
################################################################################


use_browser <-  function() {
  #############################################################
  # Opens up browser, download csv files for the list_years, renames
  # files accordingly and saves in the folder data/raw_data
  ############################################################
  
  local_path <-
    paste(str_replace_all(getwd(), "/", "\\\\"),
          "\\FL\\data\\raw",
          sep = "")
  eCaps <- list(chromeOptions =
                  list(
                    prefs = list(
                      "profile.default_content_settings.popups" = 0L,
                      "download.prompt_for_download" = FALSE,
                      "directory_upgrade" = TRUE,
                      # save downloaded files to temp raw folder
                      "download.default_directory" = local_path,
                      "safebrowsing.disable_download_protection" = TRUE
                    )
                  ))
  
  # initialize Rselenium
  rD <- rsDriver(
    browser = "chrome",
    port = 4444L,
    extraCapabilities = eCaps,
    verbose = T
  )
  remDr <- rD[["client"]]
  
  
  # navigate to main link
  link_year <- "https://results.elections.myflorida.com/"
  remDr$navigate(link_year)
  Sys.sleep(2)
  
  
  # get all frames
  frames <- remDr$findElements(using = "xpath", value = "//frame|//iframe")
  # Switch to the first frame
  remDr$switchToFrame(frames[[1]])
  # get all options from dropdown
  dropdown <-
    remDr$findElement(using = "xpath", value = "//select[@name='SelectElection']")
  options <- dropdown$findElements(using = "xpath", value = "//option")
  option_list <- options[[1]]$getElementText()
  all_options <- strsplit(as.character(option_list), "\n")[[1]]
  # remove "select" and  empty strings from the options
  all_options <- all_options[2:length(all_options)]
  all_options <- Filter(nzchar, trimws(all_options))
  
  # for all options, download the election data, with accompanying election
  # type and date
  for (i in 1:length(all_options)) {
    dropdown$clickElement()
    Sys.sleep(2)
    option_text <- all_options[i]
    # get type_election
    type_election <- ""
    if (grepl("general", all_options[i], ignore.case = TRUE)) {
      type_election <- "GEN"
    } else if (grepl("primary", all_options[i], ignore.case = TRUE)) {
      type_election <- "PRI"
    } else if (grepl("runoff", all_options[i], ignore.case = TRUE)) {
      type_election <- "RUNOFF"
    } else if (grepl("special", all_options[i], ignore.case = TRUE)) {
      type_election <- "SPE"
    }
    if (type_election == "") {
      next
    }
    option_xpath <- paste0("//option[text()='", option_text, "']")
    option <-
      remDr$findElement(using = "xpath", value = option_xpath)
    option$clickElement()
    Sys.sleep(2)
    # switch to first frame in a particular year link
    frames <- remDr$findElements(using = "xpath", value = "//frame|//iframe")
    # Switch to the first frame and get its HTML source
    remDr$switchToFrame(frames[[1]])
    Sys.sleep(1)
    frame_html <- remDr$getPageSource()[[1]]
    h <- read_html(frame_html)
    link <-
      html_node(h, xpath = "//a[contains(text(), 'Download Results')]")
    download_link <- html_attr(link, "href")
    download_link <- paste0(link_year, download_link)
    remDr$navigate(download_link)
    Sys.sleep(2)
    # click download button
    # Find the download button element using its value attribute
    download_btn <-
      remDr$findElement(using = "xpath", "//input[@value='Download']")
  
    # Click the download button
    download_btn$clickElement()
    Sys.sleep(2)
    
    # extract date from the latest saved file
    tmpshot <- fileSnapshot(local_path)
    curr_file <-
      rownames(tmpshot$info[which.max(tmpshot$info$mtime), ])
    if (grepl("raw", curr_file)) {
      print("File downloading error, previous downloads are found")
      next
    }
    # get year
    date_str <- sub("(\\d{8}).*", "\\1", curr_file)
    date <- as.Date(date_str, format = "%m%d%Y")
    year_election <-
      substr(format(date, format = "%Y"),
             start = 1,
             stop = 4)
    
    print(type_election)
    print(year_election)
    print("---------")
    
    # Rename the downloaded file accordingly
    curr_file_path <- paste0(local_path, "\\", curr_file)
    renamed_file <-
      paste0("raw_", year_election, "_", type_election, "_", date, ".csv")
    renamed_file_path <- paste0(local_path, "\\", renamed_file)
    file.rename(curr_file_path, renamed_file_path)
    print(paste("Renamed from", curr_file, "to", renamed_file))
    
    remDr$navigate(link_year)
    Sys.sleep(2)
    # get  all frames
    frames <-
      remDr$findElements(using = "xpath", value = "//frame|//iframe")
    # Switch to the first frame
    remDr$switchToFrame(frames[[1]])
    Sys.sleep(1)
    dropdown <-
      remDr$findElement(using = "xpath", value = "//select[@name='SelectElection']")
    
  }
  
  
  
  remDr$close()
}