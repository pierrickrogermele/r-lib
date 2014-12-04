if ( ! exists('read.excel')) { # Do not load again if already loaded
	
	library(rJava)
	library(xlsxjars)
	library(xlsx, quietly = TRUE)
	source('strhlp.R')
	source('dfhlp.R')
	
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
	
	# Read Excel xlsx file
	# file
	# sheet
	# start.row
	# end.row
	# header                If TRUE, use first line as header line.
	# correct.header        If TRUE, correct header (column) names in the data frame, by reading explicity the header line in the Excel.
	# remove.na.rows       If TRUE, remove all lines that contain only NA values.
	read.excel <- function(file, sheet, start.row = NULL, end.row = NULL, header = TRUE, correct.header = TRUE, remove.na.rows = TRUE) {
	
		library(rJava)
		library(xlsxjars)
		library(xlsx, quietly = TRUE)
	
		# Check that start row and end row exist
		if ( ! is.null(start.row) || ! is.null(end.row)) {
			nb_rows <- get.nbrows(file, sheet)
			if ( ! is.null(start.row) && start.row > nb_rows)
				return(NULL)
			if ( ! is.null(end.row) && end.row > nb_rows)
				return(NULL)
		}
	
		# Call xlsx package
		df <- read.xlsx(file, sheet, startRow = start.row, endRow = end.row, header = header)
	
		# Correct header (column names)
		# When reading header, read.xlsx replaces punctuation characters, accented characters and other non-ASCII characters by dot characters.
		# We try here to read directly the header line, and correct column names
		if (header && correct.header) {
			hdr <- read.xlsx(file, sheet, startRow = start.row, endRow = start.row, header = FALSE)
			hdr <- unlist(unname(hdr[1,])) # transform it into a vector of strings
			hdr <- trim(hdr) # remove spaces at beginning and end
			colnames(df) <- hdr
		}

		# Remove NA lines
		if (remove.na.rows)
			df <- remove.na.rows(df)
	
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

} # end of load safe guard
