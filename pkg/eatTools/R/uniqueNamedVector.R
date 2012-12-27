
# internal; unique Elemente eines genamten Vektors
uniqueNamedVector <- function ( namedVector ) {
		d <- data.frame ( nam = names ( namedVector ) , val = namedVector , stringsAsFactors = FALSE )
		d <- unique ( d )
		newv <- d$val
		names ( newv ) <- d$nam
		return ( newv )
}
