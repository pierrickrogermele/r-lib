source('tolst.R')

# Convert a list of key/value lists or a list of objects into a data frame. Each key becomes a column.
# x             The object to convert to data frame. Either a list of key/value lists, or a list of objects.
# rm_na_col     If true, remove all columns that contain only NA values.
todf <- function(x, rm_na_col = FALSE) {

	df <- data.frame()

	# x not null ?
	if ( ! is.null(x) && length(x) > 0) {

		# fill data frame
		for (i in 1:length(x)) {
			lst <- if (typeof(x[[i]]) == 'S4') tolst(x[[i]]) else x[[i]]
			for (k in names(lst)) {
				v <- x[[i]][[k]]
				df[i , k] <- if (length(v) > 1) paste0(v, collapse = ';') else v
			}
		}

		# remove NA columns
		if (rm_na_col) {
			drop <- character()
			for (col in names(df))
				if (all(is.na(df[[col]])))
					drop <- c(drop, col)
			if (length(drop) > 0)
				df <- df[, !(names(df) %in% drop)]
		}
	}

	return(df)
}
