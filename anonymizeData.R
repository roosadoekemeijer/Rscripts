anonymize_data <- function (dir_list, pID_col=1, change_filename = T) {
  
  # Create
  if(!dir.exists("anonymized_data")) {dir.create("anonymized_data")}
  #else{return(0)}
  
  n <- 0
  for (dir in dir_list){
    print(dir)
    file_list <- list.files(dir,recursive = F)
    file_list <- grep(".csv", file_list, value=T)
    for (file in file_list){
      n <- n + 1
      data <- data.frame(fread(
        paste0(dir, '/', file), 
        header = T, fill = T))
      if (nrow(data) == 0) {
        newfilename <- paste0("SST_data_",n,"_EMPTY.csv")
      } else {
        pID <- tail(data[,pID_col],1)
        data[,pID_col] <- n
        newfilename <- paste0("SST_data_",n,".csv")
      }
      
      # Save data (under new name)
      filename <- ifelse(change_filename, newfilename, file)
      write.table(data, paste0('anonymized_data/', filename), 
                  sep = ';', row.names = F)
    }#for file
  }#for dir
}#function

anonymize_data(dir_list)
