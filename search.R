# TODO function for searching the lower bound of a value
# TODO function for searching the higher bound of a value

binary.search <- function(val, tab, L=1L, H=length(tab)) 
{ 
	while (H >= L) { 
		M <- L + (H - L) %/% 2L 
		if (tab[M] > val) H <- M - 1L 
		else if (tab[M] < val) L <- M + 1L 
		else return(M) 
	} 
	return(L - 1L) 
} 
