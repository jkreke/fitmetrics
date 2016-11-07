##############################################################################################################################
#
# Plot the cumulative probability density function (cdf) for a given number of degrees of freedom and noise distribution function
#
#
#' Plot CDF
#'
#' Plots the Cumulative Probability Density Function
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
#' plotcdf(5, dist=uniform, fitmetric=rmse)
#'
#' @export
#' plotcdf()
plotcdf <- function(dof, order=4, dist=rnorm, fitmetric=R2, ...){  		#need to explicitly state distribiution here in order to get it into the plot title

fitmetric.character <- deparse(substitute(fitmetric))
r2cdf <- pcdfs(dof=dof, order=order, dist=dist, fitmetric=fitmetric, ...)
cdf <- NULL													#see http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when.  Need this to eliminate a note during R CMD check
N = 10^order
dist.character <- deparse(substitute(dist))
fitmetric.character <- deparse(substitute(fitmetric))
	fmet<- fitmetric.character
	gtitle<- fitmetric.character
	if(fitmetric.character=='R2'){fmet   <- expression(R^2);gtitle="R-squared"}
	if(fitmetric.character=='rmse'){fmet <- expression(RMSE);gtitle="RMSE"}
	if(fitmetric.character=='user'){fmet <- expression(user);gtitle="user"}
mxy <- max(r2cdf$cdf)
maxx <- max(r2cdf$fitval)
plot <- ggplot(r2cdf) + 
		geom_point(aes(fitval, cdf),size=1) +
		ylim(0,mxy) + 
		xlab(fmet) + 
		ylab("Cumulative Probability") +
		ggtitle(paste(gtitle,"Cumulative Probability Density Function")) +
		geom_text(aes(x=0.95*maxx,y=0.3*mxy,label=paste("Noise Distribution:",dist.character,
													"\nDegrees of Freedom:",dof,
													"\nNumber of  Samples:",floor(N))),size=3,hjust=1)

return(plot)
}