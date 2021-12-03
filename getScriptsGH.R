get.myGHscripts <- function(files="ALL", 
                         GH=c("user"="roosadoekemeijer",
                           "repos"="Rscripts",
                           "branch"="master")){
  ## Load functions from personal GitHub
  ## Input:  names of scripts on GH that need to be loaded;
  ##         GH details (username, repository, and branch)
  ## Returns: NA
  
  GHfiles <- unlist(lapply(content(GET(paste0(
    "https://api.github.com/repos/",GH['user'],"/",GH['repos'],
    "/git/trees/", GH['branch'],"?recursive=1")))$tree,"[", "path"))
  
  if(any(files=="ALL")) {files<-GHfiles}
  
  ls <- ls()
  for(f in files) {
    con <- curl(paste0(
      "https://raw.githubusercontent.com/",
      GH['user'],"/",GH['repos'],"/",GH['branch'],"/",f))
    eval(parse(text = readLines(con)))
    close(con)
  }
  loaded_f <- setdiff(ls(),ls)
  for (f in loaded_f) assign(f,get(f),envir = .GlobalEnv)
}
