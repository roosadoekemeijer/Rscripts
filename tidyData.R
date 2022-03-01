tidy_data<- function(
  data, 
  missing_label = "undefined", 
  save = F) {
  # Splits data into constants (demographics) and trial data
  # Input:  data = df; missing_label = how missings are defined;
  #         save = store the splitted data sets?
  # Output: List containing lists of pp data                             
  # Saves:  Demographics and trial data as .csv files                               
  
  #  PREP
  pIDs <- unique(data$fileID)                         # List all participant IDs
  data.demo <- data.task <- list()
  
  # RUN
  for (i in pIDs) {
    data.i <- data[which(data$fileID == i),]          # Get their data;
    # Determine make-up of all variables:
    table.i <- lapply(data.i, table)                  # List all occurrences and ...
    var.nuniq <-                                      # the number of unique values
      lapply(data.i, function(x)                      # in each column,
        length(unique(x)))
    var.w_miss <-                                     # determine whether the columns
      lapply(table.i, function(x)                     # contain any missings (logical);
        length(grep(missing_label,                
                    names(unlist(x)) )) > 0)
    
    # Select demographic variables:
    demvars.keys <- colnames(                         # Determine the names of all
      as.data.frame(data[,                            # demographics, i.e., variables 
                         var.nuniq == 1 | (                              # that have 1 value or
                           var.nuniq == 2 &                                # have 1 value and missings,
                             unlist(var.w_miss))]))
    if (length(demvars.keys) == 1) {
      demvars.keys <- "fileID"
      demvars.vals <- data.i$fileID[1]
    } else {
      demvars.keys <- grep("undefined",demvars.keys, invert = T, value = T)
      demvars.vals <-                                   # determine the (non-missing)
        unlist(lapply(                                  # values of each demographic
          data.i[,demvars.keys], function(x)              # (NB. removes demographics with
            unique(x[which(x != missing_label)])            # exclusively missing values),
        ))
    }
    data.demo.i <-                                    # transform non-empty keys
      as.data.frame(t(                                # into dataframe with their
        demvars.vals), optional = T,                    # values;
        stringsAsFactors = F)
    # Select trial data:
    data.task.i <-                                    # Determine SST data, i.e.,
      data.i[, !(colnames(data.i) %in%                # all non-demographic data;
                   demvars.keys)]
    
    # Store data sets in list:
    data.demo[i] <- list(data.demo.i)                 # Store participant's data
    data.task[i] <- list(data.task.i)                 # in the to-be-returned
    
    # Op. save data sets in files:
    if (save) {                                       # Optionally, store the
      changedpath <- FALSE                            # participant's data
      curwd <- getwd()
      if ('dataID' %in% colnames(data.i)) {      # in respective dir: either
        filepath <- data.i$dataID[1]                # indicated as their 'dataID'
        filename <- tail(str_split(                             
          filepath,"/")[[1]], 1)
        path <- str_remove(filepath,filename)         # or in the current 
        tryCatch( {                                   # directory, if the directory                         
          setwd(path)                                 # does not exist.
          changedpath <- TRUE},                               
          error = function(cond) {                    
            cat(path, "is not a directory")           
            path <- curwd })                          
      } else if ('fileID' %in% colnames(data.i)) {
        fileID <- data.i$fileID[1]
        tryCatch( {
          path <- grep(fileID, list.dirs(curwd), value = T)[1]
          setwd(path)
          changedpath <- TRUE},                               
          error = function(cond) {                    
            cat(path, "is not a directory")           
            path <- curwd })   
      } else {path <- curwd}                       
      
      cat("Writing files for\t", i, "\n")             # (actually writing files)
      write.csv(data.demo.i, file =                   
                  paste0(path,"/ParticipantDetails_",i,".csv"),
                row.names = F)
      write.csv(data.task.i, file = 
                  paste0(path,"/Trials_",i,".csv"),
                row.names = F)
      if (changedpath) {setwd(curwd)}
    }#if (save)
  }#for
  
  # DONE
  return(list('trials' = data.task,
              'pinfo' = data.demo))                 
}

