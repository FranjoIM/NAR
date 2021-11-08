# Manipulate
#
# This is a a function for manipulating nucleid acid sequence.

codonize <- function(x, reverse = FALSE, sep = " "){
  if(class(x) != "character") stop("Invalid input, variable must be character class.")
  if(class(sep) != "character") stop("Invalid input, variable must be character class.")
  if(class(reverse) != "logical") stop("Invalid input, variable must be logical class.")
  illegal <- gsub("a|t|g|c|u|A|T|G|C|U|", "", x)
  if(illegal != "") {
    warning("Illegal nucleotides found (and removed): ", paste(unique(unlist(strsplit(illegal, ""))), collapse = ", "), ".")
    x <- gsub("[^atgcuATGCU]+", "", x)
  }
  if(reverse == TRUE) {
    x
  }
  else {
    starts <- stringr::str_locate_all(x, "ATG|AUG")[[1]][,1]
    out <- list()
    for(i in 1:length(starts)){
      orf <- paste0("orf", i, collapes = "")
      y <- substr(x, starts[i], nchar(x))
      y <- paste(unlist(strsplit(y, "(?<=...)", perl = TRUE)), collapse = sep)
      end <- stringr::str_locate_all(y, "TAA|TAG|TGA|UAA|UAG|UGA")[[1]][1,2]
      tmp <- list(
        UTR3 = substr(x, 1, starts[i]-1),
        CDE = substr(y, 1, end),
        UTR5 = gsub(" ", "", substr(y, end, nchar(y)))
      )
      out[[orf]] <- tmp
    }
    out
  }
}
