##############################################################################################################################
#
# Plot the probability density function for a given number of degrees of freedom and noise distribution function
#
#
#' Plot PDF
#'
#' Plots the Probability Density Function
#'
#' @param dof an integer
#' @param order a real number
#' @param dist a random number distribution function
#' @param fitmetric a character string naming a standard fit metric (R2, rmse, or user)
#' @param ... any argument that functions within this routine might use
#'
#' @return ggplot object
#'
#' @examples
#' plotpdf(5, dist=runif, fitmetric=rmse)
#'
#' @export
#' plotpdf()
plotpdf <- function(dof, order=4, dist=rnorm, fitmetric=R2, ...){

fitmetric.character <- deparse(substitute(fitmetric))

dfx <- pcdfs(dof=dof, order=order, dist=dist, fitmetric=fitmetric, ...)
N = 10^order
dist2 <- deparse(substitute(dist))
mxy = max(dfx$pdf)
maxx <- max(dfx$fitval)
	if(fitmetric.character=="R2"){fmet   <- expression(R^2);gtitle="R-squared"}
	if(fitmetric.character=="rmse"){fmet <- expression(RMSE);gtitle="RMSE"}
	if(fitmetric.character=="user"){fmet <- expression(user);gtitle="user"}
plot <- ggplot(dfx) + 
		geom_point(aes(fitval, pdf),size=1) +
		ylim(0,mxy) +
		xlab(fmet) + 
		ylab("Probability Density") +
		ggtitle(paste(gtitle, "Probability Density Function")) +
		geom_text(aes(x=0.95*maxx,y=0.9*mxy,label=paste("Noise Distribution:",dist2,
													"\nDegrees of Freedom:",dof,
													"\nNumber of  Samples:",floor(N))),size=3,hjust=1)


return(plot)
}