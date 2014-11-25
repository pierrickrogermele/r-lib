library(rJava)
library(xlsxjars)
library(xlsx, quietly = TRUE)
source('strhlp.R')

###############
# GET NB ROWS #
###############

get.nbrows <- function(file, tab) {

	library(rJava)
	library(xlsxjars)
	library(xlsx, quietly = TRUE)

	df <- read.xlsx(file, tab)
	na_rows <- apply(is.na(df), MARGIN = 1, FUN = all) # look for rows that contain only NA values.
	last_row <- tail(which(! na_rows), n = 1)
	return(last_row)
}

##############
# READ EXCEL #
##############

# Read .xlsx file
read.excel <- function(file, sheet, startRow = NULL, endRow = NULL, header = TRUE) {

	library(rJava)
	library(xlsxjars)
	library(xlsx, quietly = TRUE)

	# Check that start row and end row exist
	if ( ! is.null(startRow) || ! is.null(endRow)) {
		nb_rows <- get.nbrows(file, sheet)
		if ( ! is.null(startRow) && startRow > nb_rows)
			return(NULL)
		if ( ! is.null(endRow) && endRow > nb_rows)
			return(NULL)
	}

	# Call xlsx package
	df <- read.xlsx(file, sheet, startRow = startRow, endRow = endRow, header = header)

	# Set header with real values.
	# If read with header = TRUE, the header names are transformed (punctuation characters (dot, space, parenthesis, star, ...) are replaced by dot character.
	if (header) {
		hdr <- read.xlsx(file, sheet, startRow = startRow, endRow = startRow, header = FALSE)
		hdr <- unlist(unname(hdr[1,])) # transform it into a vector of strings
		hdr <- trim(hdr) # remove spaces at beginning and end
#colnames(hdr) <- hdr
	}

	return(df)
}

#######################
# CHECK IF TAB EXISTS #
#######################

tab.exists <- function(file, tab) {

	library(rJava)
	library(xlsxjars)
	library(xlsx, quietly = TRUE)

	wb <- loadWorkbook(file)
	sheets <- getSheets(wb)
	return(tab %in% names(sheets))
}
