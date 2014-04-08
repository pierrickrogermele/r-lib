# Function for testing if a key exists inside a list/hashmap
hHasKey <- function(h, k) {
	return(length(which(names(h) == k)) > 0)
}

# Function for getting a boolean value from a list/hashmap
hGetBool <- function(h, k) {
	if (hHasKey(h, k)) return(h[[k]]) else return(FALSE)
}

# keys      A list of keys.
# values    A list of values.
# RETURN    A hash using keys as keys and values as values.
hCreate <- function(keys, values) {
	h <- list()
	sz <- min(length(keys), length(values))
	for(i in 1:sz)
		h[ keys[[i]] ] <- values[i]
	return(h)
}
