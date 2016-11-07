##############################################################################################################################
#
# The R-squared calculation based on two numeric vectors of equal length
#
#
#' R-squared
#'
#' Calculates R-squared given vectors for x and y
#'
#' @param x a vector or matrix of real numbers
#' @param y a vector or matrix of real numbers
#'
#' @return a vector of numbers with length equal to the number of trials (N)
#'
#' @examples
#' R2( seq(2:10),(3*rnorm(2:10)+4) )
#'
#' @export
#'R2
R2 <- function(x, y){

	mlist <- fitmetric_check(x,y)
	x <- mlist[[1]]
	y <- mlist[[2]]
	N <- mlist[[3]]
	dof <- mlist[[4]]
	
	#define R2 components x and y				
	xb		<- rowSums(x)/dof					#xbar (mean of x)
	xd		<- x-xb								#xdelta (x-xbar)

	yb 		<- rowSums(y)/dof					#ybar (mean of y)
	yd 		<- y-yb								#ydelta (y-ybar)

	#calculate R2 numerator
	n1		<- xd*yd
	n1s		<- rowSums(n1)
	num		<- n1s*n1s							#numerator

	#calculate R2 denominator
	d1		<- xd^2
	d2		<- yd^2
	d1s		<- rowSums(d1)
	d2s		<- rowSums(d2)
	den		<- d1s*d2s							#denominator

	#calculate R2 (this is an array of R2 calculations based on noise)
	out	<- num/den								#R2
	
	
	return(out)
	
	
}