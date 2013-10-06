
# sucht alle Zahlen in "range", die durch "divisors" (mit Rest 0) teilbar sind

find.devidable.number <- function ( divisors , range ) {

		f1 <- function ( number , divisors ) {
				teilbar <- ( all ( sapply ( divisors , function ( divisors , number ) number %% divisors == 0 , number ) ) )
				if ( teilbar ) return ( number ) else return ( NULL )
		}
		
		x <- sapply ( range , f1 , divisors )
		x <- unlist ( x[!sapply(x, is.null)] )
		
		return ( x )
}

# find.devidable.number ( c ( 5 , 10 , 15 , 20 , 25 , 30 , 35 ) , 1:15000 )
# find.devidable.number ( c ( 5 , 10 , 15 , 20 , 25 , 30 , 35 , 40 , 45 , 50 ) , 1:15000 )
