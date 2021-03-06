

automateModels.plot.icc <- function ( results , model.specs ) {
		
		eatTools:::sunk ( "ICC are being generated" )
		flush.console()
		
		fun <- function ( i , results , model.specs ) {

				theta <- get.person.par ( results[i] )[,c("person","pv.1")]
				# ggf. NA raus, sollte nicht aber naja
				theta <- theta[ !is.na( theta[,2] ) , ]
				
				dat <- model.specs$dataset[[i]]
				# if ( !is.null ( id.name <- model.specs$id.name[[i]] ) ) dat <- dat [ , - which ( colnames ( dat ) %in% id.name ) ]
				id.name <- model.specs$id.name[[i]]
				if ( !is.null ( cont.names <- model.specs$cont.names[[i]] ) ) dat <- dat [ , - which ( colnames ( dat ) %in% cont.names ) ]
				dat <- set.col.type ( dat , col.type = list ( "numeric" = colnames(dat)[!colnames(dat)==id.name] ) )
				
				# Datensatz reduzieren nach IDs in theta
				dat <- dat[dat[,id.name] %in% theta$person,]
				
				# theta sortieren wie dat
				theta <- theta [ match ( dat[,id.name] , theta$person ) , ]
				
				# ids droppen
				theta <- theta$pv.1
				dat <- dat <- dat [ , - which ( colnames ( dat ) %in% id.name ) , drop = FALSE ]

				# Itemparameter
				item.par <- get.item.par ( results[i] )

				# Datensatz nach item.par$items anpassen
				dat <- dat[ , colnames(dat) %in% item.par$item , drop = FALSE ]
				
				# sortieren wie in dat
				item.par <- item.par [ match ( colnames(dat) , item.par$item ) , ]

				# Parameter
				b <- item.par$b
				infit <- item.par$infit
				outfit <- item.par$outfit
				
				# eigentlich ...
				# np.dich.obj <- np.dich ( dat , theta , thetagrid , progress = F , bwscale = 1.1 , method = "binomial")
		
				# in irt.scale (z.B. verwendet f�r Nawi PilotFE)
				np.dich.obj <- np.dich ( dat , theta , thetagrid = seq( -3 , 3 , len=100) , progress = F , bwscale = 3 , method = "normal")
				flush.console()
				
				folder.icc <- file.path ( model.specs$folder[[i]] , "icc" )
				if ( !exists ( folder.icc ) ) temp <- dir.create ( folder.icc )
				
				icc.pdfs <- file.path ( folder.icc , paste ( item.par$item , ".pdf" , sep = "" ) )

				# alle in ein pdf
				pdf ( file.path ( folder.icc , "_ALL_ICC_.pdf" ) )
				temp <- plot.rasch.np ( np.dich.obj , b , infit , outfit , nsize = 100 , askplot = FALSE , progress = FALSE , 
									bands = TRUE , plot.b = TRUE , shade = FALSE , shadecol="burlywood1" , icc.pdfs = NULL )
				dev.off()
				
				# je Item ein pdf
				temp <- plot.rasch.np ( np.dich.obj , b , infit , outfit , nsize = 100 , askplot = FALSE , progress = FALSE , 
									bands = TRUE , plot.b = TRUE , shade = FALSE , shadecol="burlywood1" , icc.pdfs )

				invisible ( TRUE )
		}		
		temp <- mapply ( fun , seq ( along = results ) , MoreArgs = list ( results , model.specs ) )
		
		graphics.off()
		invisible ( TRUE )
}








