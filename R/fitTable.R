##############################################################################################################################
#
# get a boilerplate of common useful stats
#
#
#
#' Print boilerplate stats
#'
#' Prints a standard set of stats for a given degree of freedom
#'
#' @param dof an integer
#' @param pctlist a vector of real numbers between 0 and 1, minimum acceptable noise levels.
#' @param ndecimals an integer
#' @param dist a random number distribution function
#' @param order a real number
#' @param ... any argument that functions within this routine might use
#'
#' @return data frame
#'
#' @examples
#' fitStats(6, dist='normal', sd=0.3)
#'
#' @export
#' fitStats()
fitTable <- function(dof, pctlist=c(0.90,0.95,0.99), ndecimals=2, dist=rnorm, order=5, ... ) {

	R2baselines		<- NoiseTable(doflist=dof,pctlist=pctlist, order=order, fitmetric=R2,  dist=dist, ndecimals=ndecimals,...)
	RMSEbaselines	<- NoiseTable(doflist=dof,pctlist=pctlist, order=order, fitmetric=rmse,dist=dist, ndecimals=ndecimals,...)	
	userbaselines	<- NoiseTable(doflist=dof,pctlist=pctlist, order=order, fitmetric=user,dist=dist, ndecimals=ndecimals,...)
	
	np	  <- length(pctlist)
	r2b   <- unlist(R2baselines[1,])
	rmsec <- unlist(RMSEbaselines[1,])
	userd <- unlist(userbaselines[1,])
	dfx   <- data.frame(dof=rep(dof,np),percentiles=paste0(pctlist*100,"%"),R2=r2b,RMSE=rmsec, user=userd)
	rownames(dfx) <- c()
	name.width <- max(sapply(names(dfx), nchar))
	format(dfx, width = name.width, justify = "centre")
	
	print(paste("Noise Dist:         ",deparse(substitute(dist))))
	print(paste("Number of Samples:  ", 10^order))
	print(paste("Degrees of Freedom: ",dof))

	return(dfx)
}