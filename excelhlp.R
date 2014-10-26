library(xlsx)
source('strhlp.R')

##############
# READ EXCEL #
##############

# Read .xlsx file
read.excel <- function(file, sheet, startRow = NULL, endRow = NULL, header = TRUE) {

	# Call xlsx package
	df <- read.xlsx(file, sheet, startRow = startRow, endRow = endRow, header = header)

	# Set header with real values.
	# If read with header = TRUE, the header names are transformed (punctuation characters (dot, space, parenthesis, star, ...) are replaced by dot character.
	if (header) {
		header <- read.xlsx(file, sheet, startRow = startRow, endRow = startRow, header = FALSE)
		header <- unlist(unname(header[1,])) # transform it into a vector of strings
		header <- trim(header) # remove spaces at beginning and end
	}

	return(df)
}
