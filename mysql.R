library(RMySQL, quietly = TRUE)

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

# Run a insert query on a MySQL database.
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

########
# JOIN #
########

Join <- setRefClass("Join", fields = list(table = "character", left_field = "character", right_field = "character"))

Join$methods( initialize = function(table, left_field, right_field) {
	table <<- table
	left_field <<- left_field
	right_field <<- right_field
})

##########
# SELECT #
##########

# Run a select query on a MySQL database. Returns the dataframe of results.
# conn      Connection to a database.
select <- function(conn, fields, from, joins = NULL , where = NULL) {

	# Select/from
	rq <- paste("SELECT ", paste(fields, collapse = ', '), 'FROM', from)

	# Joins
	if ( ! is.null(joins) && length(joins) > 0)
		rq <- paste(rq, paste(lapply(joins, function (x) { paste('INNER JOIN', x$table, 'ON', x$left_field, '=', x$right_field) } ), collapse = ' '))

	# Where
	if ( ! is.null(where)) rq <- paste(rq, 'WHERE', where)

	# End request, send it and get results
	rq <- paste0(rq, ';')
	res <- try(dbSendQuery(conn, rq))
	data <- fetch(res, n=-1)

	return(data)
}

#######################
# SELECT SINGLE FIELD #
#######################

select_single_field <- function(conn, field, from, where = NULL) {
	values <- select(conn, fields = field, from = from, where = where)
	return(if (field %in% colnames(values) && length(values[field][[1]]) > 0) values[field][[1]] else NA_character_)
}
