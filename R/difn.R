#
#
# a difference function like diff but faster.  
# Allows one to take the difference between one value and the next or one value and the nth 
# next value (forward or backward). 
# Fills in missing values with 0 so a vector of length n results in a vector of length n.
##############################################################################################################################
#
# find the difference between two vector componenents, n places apart. Like diff, but faster and pads missing values with 0.
#
#
#' Find differences
#'
#' Finds the difference between two vector components, n places apart.  Like diff(), but faster and pads missing values with 0.
#'
#' @param x a numeric vecor
#' @param n an integer, pos or neg
#'
#' @return numeric vector
#'
#' @examples
#' difn(c(1,2,4,7,12),n=2)
#'
#' @export
#' difn()
#
difn <- function(x,n=1){
	
	if( !(class(x)=="numeric" | class(x)=="integer")  ){stop("x is not of class numeric")}
	l <- length(x)
	if(n==0){xout <- x-x}
	else if(n>0){
		l1 <- n+1
		if(n>0 & n<l){x1 <- x[c(l1:l,rep(l,n))]
						xout <- x1-x}
		}
	else if(n<0){
		m <- abs(n)
		lm <- l-m
		if(m>0 & m<l){ x1 <- x[c(rep(1,m),c(1:lm))]
						xout <- x1-x
		}
		}
	return(xout)	
}

