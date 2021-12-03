setwd_ez <- function(dir) {
  ## Find the full path to a directory to set 
  ## the working directory
  ## Input:  ...
  ## Returns: ...
  library(stringr)
  
  oldwd <- getwd()
  
  dir.steps <- strsplit(dir, "/")[[1]]
  
  # if the drive is given, just set wd traditionally
  if(str_detect(dir.steps[1],":")){setwd(dir); return(invisible())}
  
  # if possible, set wd to first directory first
  if(length(dir.steps)>1) {
    tryCatch(
      setwd_ez(dir.steps[1]), 
      error=function(e){print(paste0("Could not set wd to ",dir.steps[1]))}
    )
  }
  
  # else if only a subfolder is given, look for the folder on the current drive
  goal <- tail(dir.steps,1)
  while (length(dir.steps)) {
    dir.step <- dir.steps[1]
    curwd <- getwd()
    dirs <- list.dirs(curwd, recursive = F)
    dir.found <- grep(dir.step, dirs, value = T)
    if (length(dir.found) != 0) {
      dirs.end <- 
        lapply(dir.found, function(x) 
          tail(strsplit(x,"/")[[1]],1))
      if (any(dirs.end %in% dir.steps)) {
        cur.step <- dirs.end[dirs.end %in% dir.steps][[1]]
        while (dir.step != cur.step){
          #cat("\nFound directory ", dir.step)
          dir.steps <- dir.steps[-1]
          dir.step <- dir.steps[1]
        }
        newwd <- dir.found[dirs.end %in% dir.steps]
        setwd(newwd)
        #cat("\nFound directory ", dir.step)
        if (any(dirs.end == goal)) {
          #cat("\n\nSet working directory to ", getwd())
          return(invisible(getwd()))}
        dir.steps <- dir.steps[-1]
      }
      else {
        dirs.end <- tail(strsplit(curwd, "/")[[1]],1)
        if (dirs.end == goal) {
          cat("\n\nAlready at ", getwd())
          return(invisible(getwd()))}
        #cat("\nNo relevant subdirectories for ", dirs.end)
        setwd('../')}
    }
    else {
      #cat("\nNo directory \t", dir.step, " in ", curwd)
      setwd('../')
      if (length(strsplit(getwd(), "/")[[1]]) == 1) {
        break
      }
    }
  }#while
  setwd(oldwd)
  cat("\n\nUnable to set working directory", dir,
      "\nfor\t", oldwd)
}
