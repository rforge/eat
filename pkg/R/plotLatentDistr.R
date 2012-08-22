
plotLatentDistr <- function ( persons , items , pdf = NULL , title = NULL , scale.unit = "Logit" , person.color = "#E69F00" , item.color = "#56B4E9" , alpha = 0.5 ) {
		
		# Mittelwerte
		means <- data.frame ( "group" = c("Persons","Items") , "mean" = c(mean(persons),mean(items)) )
		
		# Farbpalette
		color <- c ( person.color , item.color )
		
		# Titel
		tit <- "Latent Distribution"
		if ( !is.null ( title ) ) tit <- paste ( tit , title , sep = "\n" )
		
		# Ns
		n.i <- length(items)
		n.p <- length(persons)
		
		# Plot erstellen
		pl <- ggplot() +
					xlab(scale.unit) + ylab("Distribution (Density)") + 
					scale_colour_manual(values=color) +			
					geom_density ( aes ( x = items, y = ..density.., fill="Items" ) , alpha = alpha ) +
					geom_density ( aes ( x = persons, y = -..density.., fill="Persons" ) , alpha = alpha ) +
					geom_vline(data=means, aes(xintercept=mean,colour=group), size=0.5) +
					coord_flip() +
					opts(title=tit) +
					guides(fill = guide_legend(reverse=TRUE)) + # reverse order of Group Levels
					scale_fill_manual(values=color,name="Group",labels=c(paste("Items N=",n.i,sep=""), paste("Persons N=",n.p,sep="")))
		
		if ( !is.null ( pdf ) ) pdf ( pdf )
		print ( pl )
		if ( !is.null ( pdf ) ) dev.off()

		invisible ( TRUE )
		
}
