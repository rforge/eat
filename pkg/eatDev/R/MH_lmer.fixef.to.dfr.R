
lmer.fixef.to.dfr <- function ( lmer.obj , long = TRUE ) {
		
		fi <- fixef ( lmer.obj )
		fi.n <- names ( fi )
		l <- capture.output ( print ( summary ( lmer.obj ) ) )

		dfr <- NULL
		for ( na in fi.n ) {

				fi.l <- which ( grepl ( "Fixed effects:" , l , fixed = TRUE ) )
				l2 <- which ( grepl ( na , l ) )
				l2 <- l2[l2>fi.l]
				string <- l [ l2 ][1]
				
				string2 <- sapply ( strsplit ( string , na , fixed = TRUE ) , "[" , 2 )
				
				as.numeric2 <- function ( x ) {
						# das hier ist natürlich etwas ungenau
						# müsste stärker elaboriert werden
						# also immer wenn nen "e" drin ist wird einfach auf 0 gesetzt
						if ( grepl ( "e" , x ) ) return ( 0 ) else return ( as.numeric ( x ) )
				}
				
				est <- as.numeric2 ( sub ( "^\\s+(\\S+)(.*)" , "\\1" , string2 ) )
				se <- as.numeric2 ( sub ( "^\\s+(\\S+)\\s+(\\S+)(.*)" , "\\2" , string2 ) )
				z <- as.numeric2 ( sub ( "\\S+(\\S+)\\s+(\\S+)\\s+(\\S+)(.*)" , "\\3" , string2 ) )
				p <- as.numeric2 ( sub ( "\\S+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)(.*)" , "\\4" , string2 ) )

				sig <- sub ( "\\S+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)(.*)" , "\\5" , string2 )
				if ( grepl ( "\\*\\*\\*" , sig  ) ) {
						sig <- 0.001
				} else if ( grepl ( "\\*\\*" , sig  ) ) {
						sig <- 0.01
				} else if ( grepl ( "\\*" , sig  ) ) {
						sig <- 0.05
				} else if ( grepl ( "\\." , sig  ) ) {
						sig <- 0.1
				} else {
						sig <- 1
				}
				
				dfr_ <- data.frame ( "variable" = rep ( na , 5 ) , "parameter" = c ( "est" , "se" , "z" , "p" , "sig" ) , "value" = c ( est , se , z , p , sig ) , stringsAsFactors = FALSE )
				
				if ( is.null ( dfr ) ) {
						dfr <- dfr_
				} else {
						dfr <- rbind ( dfr , dfr_ )
				}
			
		}

		if ( !long ) {
				dfr <- cast ( dfr , variable ~ parameter )
				dfr <- dfr [ , c ("variable" , "est", "se" , "z" , "p" , "sig" ) ]
				dfr
		}
		
		return ( dfr ) 

}
