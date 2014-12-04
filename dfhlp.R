if ( ! exists('remove.na.rows')) { # Do not load again if already loaded

	##################
	# REMOVE NA ROWS #	
	##################

	remove.na.rows <- function(df) {
		na.rows <- apply(is.na(df), MARGIN = 1, all)
		return(df[ ! na.rows, , drop = FALSE])
	}

} # end of load safe guard
