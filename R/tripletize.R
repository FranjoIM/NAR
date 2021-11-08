# Manipulate
#
# This is a a function for manipulating nucleid acid sequence.

tripletize <- transcribe <- function(x, spacing = 3, reverse = FALSE, sep = " "){
  if(class(x) != "character") stop("Invalid input, variable must be character class.")
  if(class(spacing) != "numeric") stop("Invalid input, variable must be numeric class.")
  if(class(sep) != "character") stop("Invalid input, variable must be character class.")
  if(class(reverse) != "logical") stop("Invalid input, variable must be logical class.")

  illegal <- gsub("a|t|g|c|u|A|T|G|C|U| ", "", x)
  if(illegal != "") warning("Illegal nucleotides found: ", paste(unique(unlist(strsplit(illegal, ""))), collapse = ", "), ".")

  if(reverse == TRUE) {
    gsub("[^atgcuATGCU]+", "", x)
  }

  else {
    charlen <- paste0(rep(".", spacing), collapse = "")
    charlen <- paste0("(?<=",charlen, ")", collapse = "")
    paste(unlist(strsplit(x, charlen, perl = TRUE)), collapse = sep)
    }
}
