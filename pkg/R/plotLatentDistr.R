
plotLatentDistr <- function ( persons , items , pdf = NULL , title = NULL , scale.unit = "Logit" , person.color = "#E69F00" , item.color = "#56B4E9" , alpha = 0.5 ) {
		
		# Mittelwerte
		means <- data.frame ( "group" = c("Persons","Items") , "mean" = c(mean(persons),mean(items)) )
		
		# Farbpalette
		color <- c ( person.color , item.color )
		
		# Titel
		tit <- "Latent Distribution"
		if ( !is.null ( title ) ) tit <- paste ( tit , title , sep = "\n" )
		
		# Data.frame machen
		# irgendwie kommt ggplot nicht mit Vektoren direkt zurecht
		dat.i <- data.frame(items)
		dat.i$id <- rownames(dat.i)
		dat.p <- data.frame(persons)
		dat.p$id <- rownames(dat.p)
		dat <- merge ( dat.p , dat.i , all = TRUE )
		dat$id <- NULL
		
		# Warnlevel
		oldwarn <- getOption ( "warn" )
		options ( warn = -1 )	
		
		# Plot erstellen
		pl <- ggplot(dat) +
					scale_fill_manual(values=color) +
					scale_colour_manual(values=color) +			
					geom_density ( aes(x=items,y=..density..,fill="Items") , alpha = alpha ) +
					geom_density ( aes(x=persons,y=-..density..,fill="Persons") , alpha = alpha ) +
					geom_vline(data=means, aes(xintercept=mean,colour=group), size=0.5) +
					coord_flip() +
					opts(title=tit)
		
		pl$options$labels$x <- scale.unit
		pl$options$labels$y <- "Distribution (Density)"
		pl$options$labels$fill <- "Group"
		
		if ( !is.null ( pdf ) ) pdf ( pdf )
		print ( pl )
		if ( !is.null ( pdf ) ) dev.off()

		# Warnlevel
		options ( warn = oldwarn )
		
		invisible ( TRUE )
		
}
