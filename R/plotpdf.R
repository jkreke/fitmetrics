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


dfx <- pcdfs(dof=dof, order=order, dist=dist, fitmetric=fitmetric, ...)

pdf <- NULL				#see http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when.  Need this to eliminate a note during R CMD check
N = 10^order
dist.character <- deparse(substitute(dist))
fitmetric.character <- deparse(substitute(fitmetric))
fmet <- fitmetric.character
gtitle<-fitmetric.character	
mxy = max(dfx$pdf)
maxx <- max(dfx$fitval)
plot <- ggplot(dfx) + 
		geom_point(aes(fitval, pdf),size=1) +
		ylim(0,mxy) +
		xlab(fmet) + 
		ylab("Probability Density") +
		ggtitle(paste(gtitle, "Probability Density Function")) +
		geom_text(aes(x=0.95*maxx,y=0.9*mxy,label=paste("Noise Distribution:",dist.character,
													"\nDegrees of Freedom:",dof,
													"\nNumber of  Samples:",floor(N))),size=3,hjust=1)


return(plot)
}