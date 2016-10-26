##############################################################################################################################
#
#	determine the baseline noise level (fitNoise) for a corresponding number of degrees of freedom(dof) and noise percentile(pct)
#
#
#' Find The Threshold Noise Level
#'
#' Finds the threshold noise level for a certain fit metric
#'
#' @param dof an integer
#' @param pct a real number between 0 and 1
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric ("R2", "rmse", or "user")
#' @param dist a random number distribution function
#' @param trend a character string "Positive", "Negative", "Flat", "Uncertain" which describes the general slope of the fitmetric function.
#' @param ... any argument that functions within this routine might use
#'
#' @return a real number
#'
#' @examples
#' fitNoise(7)
#' fitNoise(4, fitmetric='rmse')
#'
#' @export
#' fitNoise()
fitNoise <- function(dof, pct=0.95, ndecimals=2, fitmetric=R2, dist=rnorm, trend=NULL,...){

		if(is.null(trend)){fitmetric_trend = utrend(fitmetric)} else {fitmetric_trend=trend}
		cdf <- pcdfs(dof, ndecimals=ndecimals, fitmetric=fitmetric, dist=dist, ...)[,c("fitval","cdf")]
		if(fitmetric_trend=="Positive"){
			c_pct <- as.numeric(1-as.numeric(pct))
			c_val <- cdf$fitval[cdf$cdf<=c_pct]
			c_val <- rev(c_val)
			nb <- c_val[1]

		} else if(fitmetric_trend=="Negative") {
			nb <- cdf$fitval[cdf$cdf>=pct][1]		#nb is the value of cdf just at the point where it's just >= p
		} else {
			stop("uncertain fitmetric trend")
		}
		nb <- nb + rnorm(1)*10^(-(ndecimals+2))  #add a small random number to remove any binning errors.
		fmt <- paste0("%1.",ndecimals,"f")
		nb <- as.numeric(sprintf(fmt,nb))
	

	return(nb)
}