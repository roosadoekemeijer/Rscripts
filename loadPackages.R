loadPackages <- function(packageslist) {
  ## Loads all packages in a list
  ## Input:  list of packages names
  ## Returns: NA
  
  invisible(lapply(packageslist, function(x) {     # Load these packages.
    x <- list(x); if(!(do.call("require", x))) {
      do.call("install.packages", x)}; do.call("library", x)}))
}
