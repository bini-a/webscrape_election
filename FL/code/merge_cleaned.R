################################################################################
# File:           merge_cleaned.R
#
# Description:    merge cleaned files from data/clean 
#                 and save to data/merged
# Created by:     Biniam
################################################################################
clean_folder<- "FL/data/clean/"
merge_folder<-"FL/data/merged/"

merge_cleaned <- function() {
  ##########################################################
  ### Merge all clean csv files into single office, election,
  ##  and result files
  ##########################################################
  print("STARTED MERGING TABLES")
  cleaned_files  = list.files(clean_folder)
  for (csv_type in c("election", "office", "result")) {
    all = data.frame()
    all_files = cleaned_files[grepl(csv_type, cleaned_files)]
    for (file_name in all_files) {
      df = read_csv(paste0(clean_folder, "/", file_name), show_col_types = FALSE)
      all = rbind(all, df)
    }
    write.csv(all,file = paste0(merge_folder, csv_type,"_all.csv"), 
              row.names = FALSE)
    
  }
  print("END MERGING ALL TABLES")
  
  
}
