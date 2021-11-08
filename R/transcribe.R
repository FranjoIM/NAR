# Manipulate
#
# This is a a function for manipulating nucleid acid sequence.

transcribe <- function(x, method = "transcribe"){
  methods <- c("transcribe", "reverse.transcribe")

  if(!(method %in% methods)) stop("Please select an appropriate method.")
  if(class(x) != "character") stop("Invalid input, variable must be character class.")

  illegal <- gsub("a|t|g|c|u|A|T|G|C|U", "", x)
  if(illegal != "") warning("Illegal nucleotides found: ", paste(unique(unlist(strsplit(illegal, ""))), collapse = ", "), ".")

  if(method == "transcribe" & grepl("u|U",  x)) warning("Found \"U\" in DNA sequence.")
  if(method == "reverse.transcribe" & grepl("t|T", x)) warning("Found \"T\" in RNA sequence.")

  if(method == "transcribe") return(chartr("acgtACGT", "acguACGU", x))
  if(method == "reverse.transcribe") return(chartr("acguACGU", "acgtACGT", x))
}
