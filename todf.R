# Convert a list of key/value lists into a data frame. Each key becomes a column.
todf <- function(x) {

	df <- data.frame()

	if ( ! is.null(x) && length(x) > 0)
		for (i in 1:length(x))
			for (k in names(x[[i]])) {
				v <- x[[i]][[k]]
				df[i , k] <- if (length(v) > 1) paste0(v, collapse = ';') else v
			}

	return(df)
}
