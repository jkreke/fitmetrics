##############################################################################################################################
#
# Plot noise levels for a list of percentiles (no more than 5 in the list)
#
#
#' Plot Noise threshold
#'
#' Plots the Noise threshold for each degree of freedom
#'
#' @param doflist a vector of degrees of freedom
#' @param pctlist a vector of percentiles (expressed as fractions)
#' @param order a real number greater than 0 and less than 7
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric (R2, rmse, or user)
#' @param ... any argument that functions within this routine might use
#'
#' @return ggplot object
#'
#' @examples
#' plotNoiseLevel(doflist=c(2:10))
#'
#' @export
#' plotNoiseLevel()
plotNoiseLevel <- function(doflist=c(2:30), pctlist=c(0.95), order=4, ndecimals=2, fitmetric=R2, ...){
	if(length(pctlist)>5){stop(paste("Too many percentiles to calculate", length(pctlist)))}

	doflist <- doflist[doflist>1] 
	doflim <- min(120, length(doflist))
	doflist <- doflist[1:doflim]
	
	pctlim <- min(5,length(pctlist))
	pctlist <- pctlist[1:pctlim]
	pctlist <- formatC(as.numeric(pctlist),width=(ndecimals+1),format='f',digits=ndecimals,flag='0')
	doflength <- length(doflist)
	pctlength <- length(pctlist)
	
	mcolor <- c("black", "blue", "red", "green", "darkgreen")
	sizes <- c(3.6, 3.2, 2.8, 2.4, 2.0)/2  #make the points of the first plots larger so they can be seen
	#sizes <- c(1, 0.8, 0.6, 0.4, 0.2)   #use these for geom_path instead of geom_point
	
	fitmetric_trend = utrend(fitmetric)
	if(fitmetric_trend=="Increasing"){borc <- "Ceiling"} else {borc <- "Baseline"}
	noisedf <- Table_dofbypct(doflist=doflist, pctlist=pctlist, order=order, fitmetric=fitmetric, trend=fitmetric_trend, ...)
	mxy <- 0.9*max(noisedf[,1])
	N = 10^order
	plt <- ggplot(noisedf)

	if(length(pctlist)==1){maintitle <- paste("Noise",borc,"for \nOne Percentile(p)")
						} else {
						maintitle <- paste("Noise",borc,"for \nVarious Percentiles(p)")
						}
	fmet <- deparse(substitute(fitmetric))
	ylb <- fmet
	gtitle <- paste(fmet, maintitle)


	if(pctlength>=1){plt <- plt + 
		geom_point(aes(as.numeric(row.names(noisedf)), noisedf[,1]), color=mcolor[1], size=sizes[1])  +
		annotate("text", x=max(doflist), y=mxy-0.00, label=paste0("p = ",pctlist[1]), color=mcolor[1], hjust=1, size=4)}
	if(pctlength>=2){plt <- plt + 
		geom_point(aes(as.numeric(row.names(noisedf)), noisedf[,2]), color=mcolor[2], size=sizes[2])  +
		annotate("text", x=max(doflist), y=mxy-0.05, label=paste0("p = ",pctlist[2]), color=mcolor[2], hjust=1, size=4)}
	if(pctlength>=3){plt <- plt + 
		geom_point(aes(as.numeric(row.names(noisedf)), noisedf[,3]), color=mcolor[3], size=sizes[3])  +
		annotate("text", x=max(doflist), y=mxy-0.10, label=paste0("p = ",pctlist[3]), color=mcolor[3], hjust=1, size=4)}
	if(pctlength>=4){plt <- plt + 
		geom_point(aes(as.numeric(row.names(noisedf)), noisedf[,4]), color=mcolor[4], size=sizes[4])  +
		annotate("text", x=max(doflist), y=mxy-0.15, label=paste0("p = ",pctlist[4]), color=mcolor[4], hjust=1, size=4)}
	if(pctlength>=5){plt <- plt + 
		geom_point(aes(as.numeric(row.names(noisedf)), noisedf[,5]), color=mcolor[5], size=sizes[5])  +
		annotate("text", x=max(doflist), y=mxy-0.20, label=paste0("p = ",pctlist[5]), color=mcolor[5], hjust=1, size=4)}
	
	plt <- plt + 
	ggtitle(gtitle) +
	xlab("Degrees of Freedom") +
	ylab(ylb) +
	annotate("text", x=max(doflist), y=mxy+0.05, label=paste0("Number of Samples = ",N), color='black', hjust=1, size=4)

	
	return(plt)
}
