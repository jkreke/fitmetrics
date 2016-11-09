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
fitTable <- function(dof, pctlist=c(0.90,0.95,0.99), ndecimals=2, dist=rnorm, order=5, fitmetriclist=c(R2,rmse),... ) {

	np	  <- length(pctlist)
	dfx   <- data.frame(dof=rep(dof,np), percentiles=paste0(pctlist*100,"%") )
	fitmetric.character <- as.character(substitute(fitmetriclist))
	fitmetric.character <- fitmetric.character[2:length(fitmetric.character)]
	i=0
	for(fit.func in fitmetriclist){
		i=i+1
		fitfuncname <- fitmetric.character[i]
		fitm_lines <- NoiseTable(doflist=dof,pctlist=pctlist, order=order, fitmetric=fit.func,  dist=dist, ndecimals=ndecimals,...)
		fitm_lines2   <- unlist(fitm_lines[1,])
		nams <- names(dfx)
		dfx <- cbind(dfx,fitm_lines2)
		names(dfx) <- c(nams,fitfuncname)
	}

	rownames(dfx) <- c()
	name.width <- max(sapply(names(dfx), nchar))
	format(dfx, width = name.width, justify = "centre")
	
	print(paste("Noise Dist:         ",deparse(substitute(dist))))
	print(paste("Number of Samples:  ", 10^order))
	print(paste("Degrees of Freedom: ",dof))

	return(dfx)
}