# File:          splitFiles.R
# Description:   Splits up log files into separate datasets
# Author(s):     Doekemeijer, R. A.
# Date created:  2019-10-28
# Last modified: 2019-11-05 (fixed path param)


if (!require("data.table")){install.packages("data.table")}
if (!require("dplyr")) {install.packages("dplyr")}; library(dplyr)
if (!require("Xmisc")) {install.packages("Xmisc")}; library(Xmisc)
library(data.table)


split_file  <- function(path, filename, split_indicator, sep = "\t") {
  # Splits a file for every header into a folder of the same name as the file
  # Input:  path = working directory (string); filename = name of file (string);
  # split_indicator = element on which to split the files (string); sep = sep in files.
  # Output: N/A.
  # Saves:  folder containing sections of the original file.
  
  # SET-UP: Open connection to source and output file
  if (filename == "NA") {return()}
  wd <- getwd()
  #setwd (path); 
  s = 1 # split number
  sfilename = strsplit(filename, "[.]")[[1]][1]
  extention = paste0(".", strsplit(filename, "[.]")[[1]][2])
  sourcefile = file(paste0(path, "/", filename), "r")
  cat(">> Splitting file:", filename, "\n")
  
  if (!is.dir(sfilename)) {dir.create(sfilename)}
  splitname = paste0(sfilename, "/", sfilename, "_s", 
                     as.character(s), extention)
  if (is.file(splitname)) {file.remove(splitname)}
  file.create(splitname)
  outputfile = file(splitname, open = "a")
  
  # RUN: Split file based on lines containing split_indicator
  s = 1; cat(">> Using output file (", s, ")\n")
  while (TRUE) { 
    line = readLines(sourcefile, n = 1)
    if (length(line) == 0) {cat(">>> DONE\n"); break}
    
    # Write line to an outputfile
    line_elements = strsplit(line, sep)[[1]]
    if (split_indicator %in% line_elements) {
      s = s + 1; cat(">> Using output file (", s, ")\n")
      close(outputfile)
      
      splitname = paste0(sfilename, "/", sfilename, "_s", 
                         as.character(s), extention)
      if (is.file(splitname)) {file.remove(splitname)}
      file.create(splitname)
      outputfile = file(splitname, open = "a")
    }
    writeLines(line, outputfile)
  }
  close(sourcefile)
  close(outputfile)
  setwd(wd)
}
