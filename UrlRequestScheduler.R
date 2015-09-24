
#####################
# CLASS DECLARATION #
#####################

UrlRequestScheduler <- setRefClass("UrlRequestScheduler", fields = list(n = "numeric", t = "numeric", time_of_last_request = "ANY", useragent = "character"))

# n: number of connections
# t: time (in seconds)

# The scheduler restrict the number of connections at n per t seconds.

###############
# CONSTRUCTOR #
###############

UrlRequestScheduler$methods( initialize = function(n = 1, t = 1, useragent = NA_character_, ...) {
	n <<- n
	t <<- t
	time_of_last_request <<- -1
	useragent <<- useragent
	callSuper(...) # calls super-class initializer with remaining parameters
})

##################
# SET USER AGENT #
##################

UrlRequestScheduler$methods( setUserAgent = function(useragent) {
	useragent <<- useragent
})

##################
# WAIT AS NEEDED #
##################

# Wait the specified between two requests.
UrlRequestScheduler$methods( .wait_as_needed = function() {

	# Compute minimum waiting time between two URL requests
	waiting_time <- .self$t / .self$n

	# Wait, if needed, before previous URL request and this new URL request.
	if (.self$time_of_last_request > 0) {
		spent_time <- Sys.time() - .self$time_of_last_request
		if (spent_time < waiting_time)
			Sys.sleep(waiting_time - spent_time)
	}

	# Store current time
	time_of_last_request <<- Sys.time()
})

####################
# GET CURL OPTIONS #
####################

UrlRequestScheduler$methods( .get_curl_opts = function(url) {
	opts <- curlOptions(useragent = .self$useragent)
	return(opts)
})

###########
# GET URL #
###########

UrlRequestScheduler$methods( getUrl = function(url, params = NULL, method = 'GET') {

	library(bitops)
	library(RCurl)

	content <- NA_character_

	# Wait required time between two requests
	.self$.wait_as_needed()

	# Use form to send URL request
	if ( ! is.null(params) && ! is.na(params))
		switch(method,
		       GET = { content <- getForm(url, .opts = .self$.get_curl_opts(), .params = params) },
		       POST = { content <- postForm(url, .opts = .self$.get_curl_opts(), .params = params) },
		       stop(paste('Unknown method "', method, '".'))
		      )

	# Get URL normally
	else
		content <- getURL(url, .opts = .self$.get_curl_opts())

	return(content)
})
