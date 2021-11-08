# Manipulate
#
# This is a a function for manipulating nucleid acid sequence.

translate <- function(x, simple = FALSE, sep = "-", coding = "three"){
  if(!(class(x) %in% c("character","list"))) stop("Invalid input, variable must be character or list class.")
  if(class(sep) != "character") stop("Invalid input, variable must be character class.")
  if(class(simple) != "logical") stop("Invalid input, variable must be logical class.")
  illegal <- gsub("a|t|g|c|u|A|T|G|C|U|", "", x)
  if(illegal != "") {
    warning("Illegal nucleotides found (and removed): ", paste(unique(unlist(strsplit(illegal, ""))), collapse = ", "), ".")
    x <- gsub("[^atgcuATGCU]+", "", x)
  }

  if(class(x) == "list") {
    codons <- x
  }

  if(class(x) == "character") {
    starts <- stringr::str_locate(x, "ATG|AUG")[[1]][1]
    y <- substr(x, starts, nchar(x))
    y <- paste(unlist(strsplit(y, "(?<=...)", perl = TRUE)), collapse = sep)
    end <- stringr::str_locate(y, "TAA|TAG|TGA|UAA|UAG|UGA")[[2]][1]
    codons <- toupper(substr(y, 1, end))
  }
}
