##############################################################################################################################
#
# Generate a percentile analysis table listing baseline R2ps for various degrees of freedom and percentiles. 
#		Any measured R2 falling below these values (for the corresponding dof and pct)
#			1) are indistinguishable from noise and
#			2) will yield a negative R2k
#			3) should be discarded
#
#		This will run a call to R2p for each combination of dof and pct
#		A dof list of one dof (say,10), will take one pct just under a minute, each additional pct adds an equivalent amount (approx).
#		60 dof will take one pct about 5.75 min.
#
#		See plotR2Equiv for a plot of all R2 equal to a particular measure of R2 (with a certain dof and pct).
#
#' Noise Threshold Table
#'
#' Constructs a table in the form of a data frame of threshold noise values for a given fit metric, a given set of possible degrees of freedom and a given set of percentiles
#' 
#' @param doflist a vector of integers indicating the degrees of freedom
#' @param pctlist a vector of numbers between 0 and 1 standing for the percentile of noise
#' @param order a real number
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric (R2, rmse, or user)
#' @param trend a character string "Increasing", "Decreasing", "Flat", "Uncertain" which describes the general slope of the fitmetric function.
#' @param ... any argument that functions within this routine might use
#'
#' @return a data frame
#'
#' @examples
#' Table_dofbypct(fitmetric=rmse)
#'
#' @export
#' Table_dofbypct()
#
Table_dofbypct <- function(doflist=NULL, pctlist=NULL, order=4, ndecimals=2, fitmetric=R2, trend=NULL, ...){
	if(is.null(doflist)){doflist=c(4,8,16,32,64,128)}
	if(is.null(pctlist)){pctlist=c(0.7,0.9,0.95,0.99)}
	if(is.null(trend)){fitmetric_trend=utrend(fitmetric)} else {fitmetric_trend=trend}
	nds <- length(doflist)	#need test here for pos integer
	nps <- length(pctlist)	#need test here for nums >0 and <1
	rownams = as.character(doflist)
	colnams = as.character(pctlist)

	
	shell <- matrix(nrow=nds, ncol=nps)
	noisetab <- matrix(mapply(function(x,i,j) fitNoise(doflist[i], pctlist[j], order=order, ndecimals=ndecimals, fitmetric=fitmetric, trend=fitmetric_trend, ...), shell,row(shell),col(shell)), nrow=nds, ncol=nps)

	noisetab <- as.data.frame(noisetab)
	colnames(noisetab) <- colnams
	rownames(noisetab) <- rownams
	return(noisetab)
}