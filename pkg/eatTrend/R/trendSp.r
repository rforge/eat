trendSp <- function(meanTab1, meanTab2, linkerror) {
	
	stopifnot(all(c("group", "mean_est", "mean_se") %in% names(meanTab1)))
	stopifnot(all(c("group", "mean_est", "mean_se") %in% names(meanTab2)))
	stopifnot(identical(meanTab1[,"group"],meanTab2[,"group"]))
	trendTab <- data.frame(group=meanTab1[,"group"], stringsAsFactors=FALSE)
	trendTab$mean_trend <- meanTab2[,"mean_est"] - meanTab1[,"mean_est"]
	trendTab$mean_trend_se <- sqrt(meanTab2[,"mean_se"]^2 + meanTab1[,"mean_se"]^2 + linkerror^2)
	
	return(trendTab)

}