collapseMissings.create.recode.string <- function ( missing.rule ) {

paste ( unname ( mapply ( function ( orig , neu ) {
					if ( is.na ( neu ) ) paste ( "'" , orig , "'=" , as.character ( neu ) , sep="" ) else 
						paste ( "'" , orig , "'='" , as.character ( neu ) , "'" , sep="" )
			} , names( missing.rule ) , missing.rule , SIMPLIFY=TRUE ) ) , collapse = "; " )
}

collapseMissings <- function( dat , missing.rule = NULL , items = NULL){

	# Default-Rule
	default.rule <- list ( mvi = 0 , mnr = 0 , mci = NA , mbd = NA , mir = 0 , mbi = 0 )
 
	# Defaults setzen, bei Nutzereingabe Plausichecks
	if ( is.null ( missing.rule ) ) missing.rule <- default.rule else {
			stopifnot ( class ( missing.rule ) == "list" )
			if ( length ( missing.rule ) != length (default.rule ) ) {
				inter.rule <- default.rule
				inter.rule[names(missing.rule)] <- missing.rule
				missing.rule <- inter.rule
			}
			stopifnot ( identical ( sort ( names ( missing.rule ) ) , sort ( names ( default.rule ) ) ) )
			stopifnot ( all ( unlist ( unique ( missing.rule ) ) %in% c(0,NA) ) )
	}
	
	if (is.null (items)) {items <- colnames(dat)}

	# Plausicheck: Dataframe uebergeben?
		stopifnot ( is.data.frame ( dat ) )
		stopifnot ( !identical ( dat , data.frame() ) )
		# for ( spalte in colnames(dat) ) {
				# stopifnot ( class ( dat[,spalte] ) == "character" )
		# }
	
	# ggf. missing.rule anpassen nach variablen missing value definitionen
	
	# "Rekodieren"
	# for ( i in seq(along=missing.rule) ) {
		# dat[dat==names(missing.rule)[i]] <- as.character ( missing.rule[[i]] )
	# }

	# recode-String
	# rec.str <- paste ( unname ( mapply ( function ( orig , neu ) {
			# paste ( "'" , orig , "'='" , as.character ( neu ) , "'" , sep="" )
	# } , names( missing.rule ) , missing.rule , SIMPLIFY=TRUE ) ) , collapse = "; " )	
	rec.str <- collapseMissings.create.recode.string ( missing.rule )
	 
	# Rekodieren
	tr <- NULL
	for( i in 1:dim(dat)[2]) {tr[i] <- is.character(dat[,i]) }
	item.names.chr <- colnames(dat[,tr])[which(colnames(dat[,tr]) %in% items)]
	if(!is.null(item.names.chr)) {
		dat <- data.frame ( mapply ( function ( dat , name , item.names.chr ) {
				if ( name %in% item.names.chr ) 
					car::recode ( dat , rec.str , as.numeric = FALSE )
				else dat
			} , dat , colnames ( dat ) , MoreArgs = list ( item.names.chr ) , SIMPLIFY = FALSE ) , stringsAsFactors=FALSE )	
		} else {
		warning("None of the specified item columns is of class 'character'. collapseMissings did not collapse any missings.")
		}
		
	return ( dat )
  
 }
 
# TESTS
# missing.rule = list ( mvi = 0 , mnr = 0 , mci = NA , mbd = NA , mir = 0 , mbi = 0 )
# dat <- data.frame ( v1 = c("1", "mnr") , v2 = c("mbd","mbi") , stringsAsFactors=FALSE )
# items <- c ( "v1" , "v2" )
# str(dat)
# dat2 <- collapseMissings( dat , missing.rule = NULL , items )
# dat2
# str(dat2)

