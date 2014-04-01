##################
# OBJECT TO LIST #
##################

.object_to_list <- function(obj) {

	if(is.null(obj))
		return(NULL)

	field_names <- names(obj$getRefClass()$fields())
	l <- c()
	lapply( field_names, function(x) { l<<-c(l,list(obj$field(x))) } )
	names(l) <- field_names
	return(l)
}

###########
# TO LIST #
###########

tolst <- function(v) {

	switch(typeof(v),
	       S4 = lst <- .object_to_list(v),
	       list = lst <- v,
	       stop("Unknown type '", typeof(v), "'.")
	      )

	return(lst)
}
