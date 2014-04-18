library(RMySQL, quietly = TRUE)

#############
# RUN QUERY #
#############

# conn              The connection to the database.
# queries           A query or a list of queries.
# close             Close query with a ';' if not already done.
# RETURN            The last query result.
run_query <- function(conn, queries, close = TRUE) {

	for (query in queries) {

		# Append ';'
		if (close) {
			n <- nchar(query)
			if (substr(query, n, n) != ';')
				query <- paste0(query, ';')
		}

		# Send query
		result <- dbSendQuery(conn, query)

		# Test that everything went right
#		if ( ! dbHasCompleted(result))
#			stop("Can't run the following query : ", query)
	}

	# Return result
	return(invisible(result))
}

################
# RUN SQL FILE #
################

# conn  The connection to the DBMS.
# file  The path to the SQL file.
run_sql_file <- function(conn, file) {

	# Split SQL into single queries and put them into a list
	queries <- character()
	query <- ""
	for (line in readLines(file)) {
		line <- sub('^(.*)\\s*--.*$', '\\1', line, perl = TRUE) # remove one line comment
		if (grepl("^\\s*$", line)) next # empty line
		query <- paste(query, line)
		if (grepl(";\\s*$", line, perl = TRUE)) {
			query <- gsub("\t", " ", query, perl = TRUE) # replace tabulation by spaces
			query <- gsub("/\\*.*\\*/", "", query, perl = TRUE) # remove multiline comments
			queries <- c(queries, query)
			query <- ""
		}
	}

	# Run queries
	invisible(run_query(conn, queries))
}

#################
# DROP DATABASE #
#################

# conn                  The connection to the DBMS.
# db                    The name of the database to drop.
# fail_if_doesnt_exist  Fails if database doesn't exist.
drop_database <- function(conn, db, fail_if_doesnt_exist = FALSE) {
	invisible(run_query(conn, paste("drop database", if (fail_if_doesnt_exist) "" else "if exists", db)))
}

###################
# CREATE DATABASE #
###################

# conn      The connection to the DBMS.
# db        The name of the database to create.
# drop      Drop/erase existing database.
# encoding  Set the character set encoding to use as default for the database.
# use       If true, switch to the newly created database.
create_database <- function(conn, db, drop = FALSE, encoding = 'utf8', use = TRUE) {

	# Drop database
	if (drop) drop_database(conn, db)

	# Create database
	enc <- if (is.null(encoding) || is.na(encoding)) "" else paste("character set", encoding)
	run_query(conn, paste("create database", db, enc))

	# Switch to database
	invisible(run_query(conn, paste("use", db)))
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

Join <- setRefClass("Join", fields = list(table = "character", left_field = "character", right_field = "character", outer = "character"))

Join$methods( initialize = function(table, left_field, right_field, outer = NA_character_) {
	table <<- table
	left_field <<- left_field
	right_field <<- right_field
	outer <<- outer
})

Join$methods( getStatement = function() {
	type <- 'INNER JOIN'
	if ( ! is.na(outer))
		switch(tolower(outer),
		       left  = type <- 'LEFT OUTER JOIN',
		       right = type <- 'RIGHT OUTER JOIN',
		       stop('Error in join outer type. "', outer ,'" is unknown. You must choose between "LEFT" and "RIGHT".')
		      )

	return(paste(type, .self$table, 'ON', .self$left_field, '=', .self$right_field))
})

##########
# SELECT #
##########

# Run a select query on a MySQL database. Returns the dataframe of results.
# conn      Connection to a database.
select <- function(conn, fields, from, joins = NULL , where = NULL, orderby = NULL) {

	# Select/from
	rq <- paste("SELECT ", paste(fields, collapse = ', '), 'FROM', from)

	# Joins
	if ( ! is.null(joins) && length(joins) > 0)
		rq <- paste(rq, paste(lapply(joins, function (x) x$getStatement() ), collapse = ' '))

	# Where
	if ( ! is.null(where)) rq <- paste(rq, 'WHERE', where)

	# Order by
	if ( ! is.null(orderby)) rq <- paste(rq, 'ORDER BY', orderby)

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
	val <- if (field %in% colnames(values) && length(values[field][[1]]) > 0) values[field][[1]] else NA_character_
	return(val)
}
