
shade.between.lines <- function ( d ) {
		
		require ( plyr )
		
		### Aufbereitung des Konfidenzbanddatensatzes
		# http://mcfromnz.wordpress.com/2014/06/02/shading-between-two-lines-ggplot/
		# Levels für die Polygon-Gruppe: müssen so viele sein wie Werte auf x-Achse
		# group.lev <- make.unique ( rep_len ( letters , length.out = length ( unique(pred_confidenceband$score) ) ) )
		gl <- seq ( along = unique(d[,1]) )
		group.lev <- formatC ( gl , format = "f", flag = "0" , digits = 0 , width = max ( sapply ( gl , nchar ) ) )
		
		# Check ob von jeder Gruppe 2 da
		unique.el <- sort ( unique ( d[,1] ) )
		unique.el.l <- sapply ( unique.el , function ( x , y ) nrow ( y[ y[,1] %in% x, , drop = FALSE ] ) , d )
		not2 <- unique.el[!unique.el.l == 2]
		not2.l <- unique.el.l[!unique.el.l == 2]
		if ( !identical ( not2 , integer(0) ) ) {
				cat ( paste0 ( "Group levels without 2 elements: " , paste ( paste0 ( not2 , " (" , not2.l , ")" ) , collapse = ", " ) , "\n" ) )
		}
		
		mperson <-function(x,group.lev) {
			x <-x[order(x$score), ]
			y <-x[-c(1, 2, nrow(x) -1, nrow(x)), ]
			x <-rbind(x, y)
			x <-x[order(x$score), ]
			x$polygr.group <-rep(group.lev[1:(nrow(x)/4)], each = 4)
			return(x)
		}
		d2 <- mperson ( d , group.lev)

		mgroup <-function(x) {
			x <-x[order(x[,2]), ]
			left <-x[x[,1] ==min(x[,1]), ]
			right <-x[x[,1] ==max(x[,1]), ]
			if (all(left$order ==right$order)) {
				left <-left[order(left[,2], decreasing = TRUE), ]
				right <-right[order(right[,2], decreasing = FALSE), ]
				return(rbind(left, right))
			} else {
				return(x[order(x$score), ])
			}
		}
		d3 <- ddply ( d2, .(polygr.group), mgroup)
		
		# später zum sortieren
		d3$polygr.sort <- as.integer ( seq ( along = rownames ( d3 ) ) )
		
		return ( d3 )
}