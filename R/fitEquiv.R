##############################################################################################################################
#
#	For a given value of R2, dof and pct, determine the noise-normalized, dof-independent, 
#      distribution-independent, R2 equivalent:  R2k
#
#' Fit Equivalent
#' 
#' Rescales the fit value to distance to the threshold
#'
#' @param measured_value a real number
#' @param dof an integer
#' @param pct a real number between 0 and 1
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric (R2, rmse, or user)
#' @param trend a character string "Positive", "Negative", "Flat", "Uncertain" which describes the general slope of the fitmetric function.
#' @param ... any argument that functions within this routine might use
#'
#' @return a real number
#'
#' @examples
#' fitEquiv(0.8, 6)
#' fitEquiv(0.1, dof=8, fitmetric=rmse)
#'
#' @export
#' fitEquiv()
fitEquiv <- function(measured_value, dof, pct=0.95, ndecimals=2, fitmetric=R2, trend=NULL, ...){
	fitval=measured_value
	noiselevel <- fitNoise(dof=dof, pct=pct, ndecimals=ndecimals, fitmetric=fitmetric, trend=trend, ...)

	#determine if noise trends upward (positive) or downward (negative)
	if(is.null(trend)){fitmetric_trend = utrend(fitmetric)} else {fitmetric_trend=trend}
	if(fitmetric_trend=="Negative"){
			eqfitval <- (fitval-noiselevel)/(1-noiselevel + 0.00000000001)
		} else if(fitmetric_trend=="Positive"){
			eqfitval <- fitval/noiselevel
		} else 
			stop("uncertain fitmetric trend")
	
	
	#make eqfitval consistent with the number of decimal places in noiselevel.  may have to tweak this for positive trend functions
	fl <- floor(noiselevel)								
	nd <- rep(0,length(fitval))
	if(noiselevel-fl>0){
		nd <- nchar(sapply(strsplit(as.character(noiselevel), ".",fixed=T), "[[", 2))} 
	fmt <- paste0("%1.",ndecimals,"f")
	eqfitval <- as.numeric(sprintf(fmt,eqfitval))

	return(eqfitval)
}