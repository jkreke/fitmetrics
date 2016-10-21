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

	gclasses <- c("numeric", "integer")
	if(class(x) %in% gclasses & class(y) %in% gclasses & length(x)==length(y)){
		x <- matrix(x,nrow=1)
		y <- matrix(y,nrow=1)
		} else if(class(x)=="matrix" & class(y)=="matrix"){
			
			
		} else {
			stop("x & y classes or lengths different")
		}
	if(ncol(x)==1 | ncol(y)==1){stop("can not compute R2 with only on value")}

	if(class(x)     == "matrix" & 
	   class(y)     == "matrix"  &
	   dim(x)[[1]]  == dim(y)[[1]]  & 
	   dim(x)[[2]]  == dim(y)[[2]]
	   ){		
		N=dim(x)[[1]]
		dof=dim(x)[[2]]
		}
	
	
	#define R2 components x and y				
	xb		<- rowSums(x)/dof
	xd		<- x-xb

	yb 		<- rowSums(y)/dof					#get means for each row
	yd 		<- y-yb								#get delta

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
	R2vector	<- num/den						#R2
	
	
	
	return(R2vector)
	
	
}