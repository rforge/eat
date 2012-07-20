
make.link.dummy <- function ( dfr , analysis.name = "dummy.analysis" ) {

		data(ex5)

		dummy <- ex5[1]

		make.i.dummy <- function ( item , b , b.se , dummy) {
				i.dummy <- dummy[[1]][[1]][[1]][[1]][1]
				i.dummy[[1]]$n.valid = 0
				i.dummy[[1]]$p = 0
				i.dummy[[1]]$a = 0
				i.dummy[[1]]$b = b
				i.dummy[[1]]$c = 0
				i.dummy[[1]]$d = 0
				i.dummy[[1]]$b.se = b.se
				i.dummy[[1]]$infit = 0
				i.dummy[[1]]$infit.ci.lb = 0
				i.dummy[[1]]$infit.ci.ub = 0
				i.dummy[[1]]$infit.t = 0
				i.dummy[[1]]$outfit = 0
				i.dummy[[1]]$outfit.ci.lb = 0
				i.dummy[[1]]$outfit.ci.ub = 0
				i.dummy[[1]]$outfit.t = 0
				i.dummy[[1]]$pbc = 0
				i.dummy[[1]]$b.eval = 0
				i.dummy[[1]]$infit.eval = 0
				i.dummy[[1]]$pbc.eval = 0
				i.dummy[[1]]$eval.num = 0
				i.dummy[[1]]$eval = 0 
				names(i.dummy) <- item
				return (i.dummy)
		}

		i.dummies <- mapply ( make.i.dummy , dfr$item , dfr$b , dfr$b.se , MoreArgs = list ( dummy ) , SIMPLIFY = FALSE )

		do <- paste ( "dummy[[1]][[1]][[1]][[1]]<-c(" , paste ( "i.dummies[[" , seq(along=i.dummies) , "]]" , collapse = "," ) , ")" , sep = "" )
		eval ( parse ( text = do ) )
		
		names(dummy) <- analysis.name
		names(dummy[[1]]) <- "dummy.dim"
		names(dummy[[1]][[1]]) <- "dummy.group"
		
		return ( dummy )

}
