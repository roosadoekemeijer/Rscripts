# File:          splitFiles.R
# Description:   Splits up log files into separate datasets
# Author(s):     Doekemeijer, R. A.
# Date created:  2019-10-28
# Last modified: 2019-11-05 (fixed path param)

get_data <- function(path, filepattern, ignorepattern = NA, sep="\t",
                     fileIDind = 1, split_indic = "Event Type", verbose = F) {
  # Selects files and extracts log data from a given working directory
  # Input:  path = working directory (string); verbose =  print info?
  # Output: single dataframe (w/ ppID and dataID)
  
  # List all relevant directories
  dir_list <- list.dirs(path); dir_list <- dir_list[2:length(dir_list)]
  
  # Check if there are files that have not yet been splitted
  # If so, run the split again
  pp_list <- list.files(path, pattern = filepattern)
  if (length(pp_list) == 0) {return(0)}
  ppd_list <- paste0(path, "/", pp_list)
  ppd_list <- substr(ppd_list, 1, nchar(ppd_list)-4)
  if (!(all(ppd_list %in% dir_list))) {
    logfiles <- list.files(path, pattern = filepattern)
    for (i in logfiles) {split_file(path, i, split_indic, sep)}
    dir_list <- list.dirs(path); dir_list <- dir_list[2:length(dir_list)]
  }#if
  
  
  # List and import relevant files
  log_files <- list(); n=0
  for (i in dir_list) {
    log_file <- paste0 (i, "/", list.files(i, pattern = filepattern))
    log_file <- log_file[grep(ignorepattern, log_file, invert = T)]
    if(length(log_file) > 2) {n=n+1}#;cat("\n", i, " has ", length(log_file)-2, " additional data set(s)!")}
    log_files <- append(log_files, tail(log_file,1))
  }#for
  
  log_data <- lapply(log_files, fread, header = TRUE, fill=TRUE)
  
  
  # Bind all into single dataframe, while keeping dataID & subjectID
  for (i in (1:length(log_data))) { ## take the last file in each dir
    log_data[[i]] <- log_data[[i]]#[,1:32]
    subjectID <- substr(dir_list[i], nchar(path) + 2, nchar(dir_list[i]))
    log_data[[i]][,"dataID"] <- log_files[i]
    log_data[[i]][,"fileID"] <- strsplit(subjectID, "_")[[1]][fileIDind]
  }#for
  df <- as.data.frame(do.call("rbind", c(log_data, fill = TRUE)))
  df <- df[,1:grep("fileID", colnames(df))]
  
  
  # Op. Print information
  if (verbose) {cat("\n*** IMPORTED DATA ***",
                    "\n > Subject IDs: ", unique(df$fileID), 
                    "\n  Tot:\t\t", length(unique(df$fileID)),
                    "\n***",
                    "\n>",n, "participant(s) had too much data",
                    "\n***\n")}
  
  return(df)
  
}
