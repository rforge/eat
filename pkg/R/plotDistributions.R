
plotDistributions <- function ( distr1 , distr2 , distr1.name = "Persons" , distr2.name = "Items" , pdf = NULL , title = NULL , scale.unit = "Logit" , distr1.color = "#E69F00" , distr2.color = "#56B4E9" , alpha = 0.5 ) {
		
		# ggplot2 ist irgendwie buggy oder suboptimal
		# wenn auf GlobalEnv nicht das Objekt existiert
		# funktionierts mit ggplot(<leer>) in einer Funktion nicht
		# der hotfix hier ist auch suboptimal, aber was solls
		# (so suboptimal/ganz schlimm ists auch nicht)
		.GlobalEnv$distr1 <- distr1
		.GlobalEnv$distr2 <- distr2
		
		# Mittelwerte
		means <- data.frame ( "group" = c("distr1","distr2") , "mean" = c(mean(distr1),mean(distr2)) )
		
		# Farbpalette
		color <- c ( distr1.color , distr2.color )
		
		# Titel
		tit <- "Latent Distribution"
		if ( !is.null ( title ) ) tit <- paste ( tit , title , sep = "\n" )
		
		# Ns
		n.distr2 <- length(distr2)
		n.distr1 <- length(distr1)
		
		# Plot erstellen
		pl <- ggplot() +
					xlab(scale.unit) + ylab("Distribution (Density)") + 
					scale_colour_manual(values=color) +			
					geom_density ( aes ( x = distr2, y = ..density.., fill="distr2" ) , alpha = alpha ) +
					geom_density ( aes ( x = distr1, y = -..density.., fill="distr1" ) , alpha = alpha ) +
					geom_vline(data=means, aes(xintercept=mean,colour=group), size=0.5) +
					coord_flip() +
					opts(title=tit) +
					# guides(fill = guide_legend(reverse=TRUE)) + # reverse order of Group Levels
					scale_fill_manual(values=color,name="Group",labels=c(paste(distr1.name," N=",n.distr1,sep=""), paste(distr2.name," N=",n.distr2,sep="")))
		
		if ( !is.null ( pdf ) ) pdf ( pdf )
		print ( pl )
		if ( !is.null ( pdf ) ) dev.off()

		invisible ( TRUE )
		
}
