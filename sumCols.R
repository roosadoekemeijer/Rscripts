# File:          summarizeCols.R
# Description:   Computes (and writes) summary of cols split over multiple filenames
# Author(s):     Doekemeijer, R. A.
# Date created:  2019-11-08
# Last modified: 2019-11-08


if (!require("data.table")){install.packages("data.table")}
if (!require("dplyr")) {install.packages("dplyr")}; library(dplyr)
if (!require("Xmisc")) {install.packages("Xmisc")}; library(Xmisc)
library(data.table)


sumCols <- function(df, write_file = NA) {
  # Computes summary for each numerical column in a .csv file and 
  # writes (appends) it to an (existing) .csv file, if desired
  # Input:  path = working directory (string); filenames = name(s) of file(s)
  # containing to be summarized columns (string); write_file = if summary
  # is to be stored, outputfilename; otherwise, NA.
  # OutputL N/A.
  # write_files: M, SD, min, max in an .csv file, if desired.
  
  # create file, if it doesn't exist yet, or open existing file
  if (!(is.na(write_file))) {
    if (!(is.file(write_file))) { # create file
      file.create(write_file)
      outputfile <- file(write_file, open = "w")
      writeLines(c("ppID", "par", "M", "SD", "Med", "Min", 
        "2.5%", "25%", "50%", "75%", "97.5%", "Max"), outputfile, sep = ";")
      presentpar <- c(NA)
    } else { # or open file
      outputfile <- file(write_file, open = "a")
      presentpar <- read.table(write_file, sep = ";", header = T)$par
    }
  }
  
  for (i in colnames(df)) {
    df.i <- df[,i]
    parID <- strsplit(i, "[.]")[[1]][1]
    ppID <- strsplit(i, "[.]")[[1]][2]
    line <- c(ppID, parID,  mean(df.i), sd(df.i), median(df.i), min(df.i), 
        quantile(df.i, c(0.025,0.25,0.5,0.75,0.975)), max(df.i))
    if (!(is.na(write_file))) {
      if (!(parID %in% presentpar)) {
        writeLines("\r", outputfile)
        writeLines(line, outputfile, sep = ";")}
    } else {cat("\n", line)}
  }#for
  
  if (!(is.na(write_file))) {close(outputfile)}
}
