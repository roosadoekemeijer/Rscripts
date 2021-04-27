# File:          mkLatexTable.R
# Description:   makes fancier overview of results and assumption/pos.pred. checks
# related files: wrap_ExtrinsicMotivation
# Author(s):     Doekemeijer, R. A
# Date created:  2020-04-14
# Last modified: 2020-04-14

toLaTeXtable <- function(dataframe, caption = "CAPTION", greek = NA) {
  
  ## Format all names
  # Column names
  colnames(dataframe) <- gsub("%", "\\\\%", colnames(dataframe))
  
  if (any(grep(":", colnames(dataframe)))) { # there is a subheader
    all <- str_split(colnames(dataframe), ":")
    header <- sapply(all, function(x) {x[1]})
    subheader <- sapply(all, function(x) {x[2]})
    
    headerline <- ""
    multicollengths <- list()
    for (i in unique(header)) {
      multicollength <- sum(str_ends(header, i))
      if (multicollength != 0) {headerline <- paste0(headerline, "\\multicolumn{", multicollength, "}{c}{", i, "} & & ");
      multicollengths <- append(multicollength, multicollengths)
      } else {headerline <- paste0(headerline, "\\multirow{2}{*}{", i, "} & ")}
    }
    ind <- unlist(multicollengths) + c(0,unlist(multicollengths[-length(multicollengths)]))
    subheaderline <- paste0(subheader, " & "); subheaderline[ind] <- paste0(subheaderline[ind], " & ")
    subheaderline <- paste0(subheaderline, collapse = "")
    subheaderline <- gsub("NA", "", subheaderline)
  }
  
  # Both
  names <- list(colnames(dataframe), rownames(dataframe))
  names <- lapply(names, function(x) {
    subsc_ind <- grep("_", x)
    if (length(subsc_ind)) {x <- gsub("_", "_{", x); x[subsc_ind] <- paste0(x[subsc_ind], "}")}
      
    greek_ind <- grep(paste0(greek, collapse = "|"), x)
    if (length(greek_ind)) {x[greek_ind] <- paste0("\\", x[greek_ind])}
    
    for (i in unique(c(greek_ind, subsc_ind))) {x[i] <- paste0("$", x[i], "$")}
    return(x) })
  colnames(dataframe) <- names[[1]]; rownames(dataframe) <- names[[2]]
  
  
  # Both
  
  # Begin table
  cat("\\begin{table*}[!h]\n",
                   "\\caption{\\label{Table:NAME}", caption, "}\n",
                   "\\small \n",
                   "\\newlength{\\colw}\\setlength{\\colw}{", 10/ncol(dataframe),"cm}\n",
                   "\\begin{tabularx}{\\textwidth}{L{0.9cm} ")
  for(i in multicollengths) {cat(rep("R{\\colw}", i), "C{0.1cm} ")}
  cat(rep("R{\\colw}", length(unique(header))-length(multicollengths)), "}\n \\toprule \n")
  
  # Header
  cat(" & ", headerline, "\n"); start <- 2;
  for (i in multicollengths) {cat("\\cline{", start, "-", start+i-1,"}"); start <- start+i+1};
  cat("\n & ", subheaderline, "\n")
  
  
  # Table corpus
  for (i in 1:nrow(dataframe)){
    line <- array(dataframe[i,]); key <- rownames(line)
    line <- sapply(line, function(x) {
      n <- str_length(as.character(as.integer(x)))
      x <- round(as.numeric(x), 2)
      if (!(length(grep("[.]", x)))) {x <- paste0(x, ".")}
      x <- str_pad(x, n + 3, "right", 0)
      })
    line[ind] <- paste0(line[ind], " & ")
    cat(key, " & ",  paste0(line, " & "), '\n')
  }
  
  # End table
  cat("\\bottomrule \n \\end{tabularx} \\end{table*}\n\n\n")
}