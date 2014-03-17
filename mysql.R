library(DBI)
library(RMySQL)

#############
# RUN QUERY #
#############

run_query <- function(conn, query) {
	result <- dbSendQuery(conn, query)
	if ( ! dbHasCompleted(result)) {
		print("Can't run the following query :")
		print(query)
		exit(1)
	}
}

##############################
# CONVERT VALUE TO SQL VALUE #
##############################

to_sql_value <- function(x) {

	# NA or NULL
	if (length(x) == 0 || is.na(x) || is.null(x))
		return('null')

	# String
	if (is.character(x))
		return(paste0('"', as.character(x), '"'))

	return(x)
}

####################
# MAKE INSERT LINE #
####################

make_insert_line <- function(values) {
	values <- lapply(values, to_sql_value)
	return(paste0("(", paste(values, collapse=','), ")"))
}

##########
# INSERT #
##########

# conn      Connection to a database.
# table     Table name.
# fields    List of field names.
# values    List of list of values. NA values will be translated as NULL.
insert <- function(conn, table, fields, values) {

	# Do nothing if no values
	if (length(values) == 0 ) return

	# Build header
	h <- paste("insert into", table)
	h <- paste0(h, "(", paste(fields, collapse = ','), ")")
	h <- paste(h, "values")

	qr <- paste(h, paste0(lapply(values, make_insert_line), collapse=','), ';')

	# Send query
	run_query(conn, qr)
}
