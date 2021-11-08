# Manipulate
#
# This is a a function for manipulating nucleid acid sequence.

manipulate <- function(x, method = "reverse.complement"){
  methods <- c("reverse", "complement", "reverse.complement")

  if(!(method %in% methods)) stop("Please select an appropriate method.")
  if(class(x) != "character") stop("Invalid input, variable must be character class.")

  illegal <- gsub("a|t|g|c|A|T|G|C", "", x)
  if(illegal != "") warning("Illegal nucleotides found: ", paste(unique(unlist(strsplit(illegal, ""))), collapse = ", "), ".")

  if(method == "reverse") return(stringi::stri_reverse(x))
  if(method == "complement") return(chartr("acgtACGT", "tgcaTGCA", x))
  if(method == "reverse.complement") return(stringi::stri_reverse(chartr("acgtACGT", "tgcaTGCA", x)))
}
