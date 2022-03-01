mod_miss <- function(
  df, 
  miss.curr = NA, miss.des = "-999",
  miss.lim = 9999, verbose = F) {
  # Relabels [miss.cur] as [miss.des], removes 'completely' empty lines
  # (i.e., rows that exceed miss.lim)
  # Input:  df = data w/ missings; miss.cur = current label of missings;
  # miss.mod = desired label of missings; miss.lim = limit of max missings.
  # Output: dataframe with relabeled and/or less missings.
  
  # Remove rows that are considered 'completely' empty
  empty_rows <- rep(F, nrow(df))
  if (miss.lim < ncol(df)) {for (i in 1:nrow(df)) {
    if (sum(is.na(df[i,])) > miss.lim) {empty_rows[i] <- T}}}#if#for#if
  df.nna <- df[!(empty_rows),]
  
  
  # Relabel missings
  miss.ntot <- 0
  for (i in 1:ncol(df)) {
    miss.n <- 0
    if (is.na(miss.curr)) {
      miss.n <- sum(is.na(df.nna[i]))
      df.nna[is.na(df.nna[i]), i] <- miss.des
    } else if (miss.curr %in% df.nna[,i]) {
      miss.n <- sum(df.nna[i] == miss.curr)
      df.nna[which(df.nna[i] == miss.curr), i] <- miss.des
    }#elseif
    miss.ntot <- miss.ntot + miss.n
    if (verbose) {cat("\n  Changed", miss.n, "values in column ", colnames(df)[i])}
  }#for
  
  # Op. Print information
  if (verbose) {cat("\n\n*** MISSING INFO ***",
                    "\n > All rows:", nrow(df), 
                    "\n > Nonempty rows:", nrow(df.nna),
                    "\n > Empty rows:", sum(empty_rows),
                    "\n > Missing data:", miss.ntot,
                    "\n***\n")}
  
  return(df.nna)
  
}

