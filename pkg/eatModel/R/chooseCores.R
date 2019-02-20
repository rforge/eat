chooseCores <- function(cores = NULL, GBcore = NULL, max.cores = NULL ) {
             # if(!exists("detectCores"))   {library(parallel)}
             n.cores <- detectCores()
             if(is.na(n.cores))     {cat("Cannot detect cores. Computer does not seem to be suited for multicore analysis.\n")}
             ram     <- memory.limit()
             if(!is.null(cores)) {
                use.cores <- as.integer(cores)
                # if(use.cores == 1 ) {cat("Not useful to choose 1 core in multicore option.\n")}
                if(use.cores > n.cores) {
                   cat(paste("Fail to use ", use.cores," cores. Found only ",n.cores," cores which will now be purposed to use.\n",sep=""))
                   use.cores <- n.cores
                }
             } else { use.cores <- n.cores }
             if(!is.null(GBcore)) {
                ramGB <- ram/1024
				if(GBcore > ramGB ) {
                   cat(paste("Warning: Not able to use ",GBcore, " giga byte per core. Only ",round(ramGB,digits = 2)," giga byte found altogether.\n",sep=""))
                   GBcore <- ramGB
                }
                use.cores.new <- (ramGB) / GBcore
                if(use.cores.new < use.cores ) {use.cores <- use.cores.new}
             }
			 if (!is.null(max.cores)) {
					use.cores <- min( use.cores, max.cores )
			 } 
			 return( use.cores ) }
