source("tolst.R")

# Convert lists and objects to string representation. Supported outputs are:
#   _ Text.
#   _ PHP code.
#   _ R code (to be done).

##########################
# SET STRING TO VARIABLE #
##########################

# str   The value converted to a string.
# mode  The mode to use.
# var   Variable name.
.set_str_to_variable <- function(str, mode = 'txt', var = NA_character_) {

	# Add variable
	switch(mode,
	       txt = { str <- paste(var, '=', str) },
	       php = { str <- paste0('$', var, ' = ', str, ';') },
	       stop("Unknown mode '", mode, "'.")
		  )

	return(str)
}

################
# QUOTE VALUES #
################

# values    A vector of values.
# mode      The mode to use.
# keys      If the vector contains keys of a dictionary structured (depending on the mode, they will be quoted or not).
.quote_values <- function(values, mode = 'txt', keys = FALSE) {

	if (mode == 'txt' && keys)
		return(values)

	# Quote string values
	# TODO escape quote characters
	if (is.character(values))
		return(unlist(lapply(values, function(x) { paste0('"', x, '"') } )))

	return(values)
}

############
# SET KEYS #
############

# values    Vector or list of values.
# mode      The mode to use.
.set_keys <- function(values, mode = 'txt') {

	if ( ! is.null(names(values))) {
		keys <- names(values)
		indices <- 1:length(values)
		switch(mode,
	       	   txt = { values <- lapply(indices, function(x) paste( if (nchar(keys[[x]]) == 0) x else keys[[x]], '=>', values[[x]])) },
	       	   php = { values <- lapply(names(values), function(x) paste0('"', if (nchar(keys[[x]]) == 0) x else keys[[x]], '"', ' => ', values[[x]])) },
	       	   stop("Unknown mode '", mode, "'.")
		  	  )
	}

	return(values)
}

###############
# JOIN VALUES #
###############

# values    Vector or list of values to join.
# mode      The mode to use.
.join_values <- function(values, mode = 'txt') {

	switch(mode,
	       txt = { str <- paste0('(', paste(values, collapse = ', '), ')') },
	       php = { str <- paste0('[', paste(values, collapse = ', '), ']') },
	       stop("Unknown mode '", mode, "'.")
		  )

	return(str)
}

###############
# NULL TO STR #
###############

# value The NULL or NA value, or the vector of length 0.
# mode  The mode to use.
# var   Variable name.
.null_to_str <- function(value, mode = 'txt', var = NA_character_) {

	# Set to 'null' string
	switch(mode,
	       txt = { str <- if (length(value) > 0 && is.na(value)) 'NA' else 'null' },
	       php = { str <- 'NULL' },
	       stop("Unknown mode '", mode, "'.")
		  )

	if ( ! is.null(var) && ! is.na(var))
		str <- .set_str_to_variable(str, mode, var)

	return(str)
}

################
# VALUE TO STR #
################

# TODO hide this function ? value_to_str -> .value_to_str

# value The value to convert.
# mode  The mode to use.
# var   Variable name.
# lst   If true, print the output as a list or array, even if it contains only one value.
.value_to_str <- function(value, mode = 'txt', var = NA_character_, lst = FALSE) {

	if (is.null(value) || (length(value) == 0 && ! lst) || (length(value) > 0 && is.na(value)))
		return(.null_to_str(value, mode = mode, var = var))

	# Transform value to a string
	value <- .quote_values(value, mode = mode)
	str <- if (length(value) == 1 && ! lst && is.null(names(value))) as.character(value) else .join_values(.set_keys(value, mode = mode), mode = mode)

	# Set to variable
	if ( ! is.null(var) && ! is.na(var))
		str <- .set_str_to_variable(str, mode, var)

	return(str)
}

###############
# LIST TO STR #
###############

# vlist The list to convert.
# mode  The mode to use.
# var   Variable name.
# lst   If true, print the output as a list or array, even if it contains only one value.
.list_to_str <- function(vlist, mode = 'txt', var = NA_character_, lst = FALSE) {

	if (is.null(vlist) || (length(vlist) == 0 && ! lst) || (length(vlist) > 0 && is.na(vlist)))
		return(.null_to_str(vlist, mode = mode, var = var))

	# 
	vstr <- character()
	if (length(vlist) > 0) {
		keys <- unlist(lapply(names(vlist), function(x) if (nchar(x) == 0) x else .quote_values(x, mode = mode, keys = TRUE)))
		values <- lapply(vlist, function(x) tostr(x, mode = mode))
		sep <- switch(mode,
	              	  txt = '=>',
	              	  php = '=>',
		          	  stop("Unknown mode '", mode, "'.")
		         	 )
		vstr <- unlist(lapply(1:length(vlist), function(i) if (is.null(keys) || nchar(keys[i]) == 0) values[[i]] else paste(keys[i], sep, values[[i]])))
	}

	# Join string values
	if (length(vstr) > 1 || lst || ! is.null(keys))
		str <- .join_values(vstr, mode = mode)
	else
		str <- vstr
			
	# Set to variable
	if ( ! is.null(var) && ! is.na(var))
		str <- .set_str_to_variable(str, mode, var)

	return(str)
}

##########
# TO STR #
##########

# obj   The object to convert.
# mode  The mode to use.
# var   Variable name.
# lst   If true, print the output as a list or array, even if it contains only one value.
tostr <- function(obj, mode = 'txt', var = NA_character_, lst = FALSE) { 

	switch(typeof(obj),
	       S4   = str <- tostr(tolst(obj), mode = mode, var = var, lst = lst),
	       list = str <- .list_to_str(obj, mode = mode, var = var, lst = lst),
	              str <- .value_to_str(obj, mode = mode, var = var, lst = lst)
	      )
		
	return(str)
}
