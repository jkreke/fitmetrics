##############################################################################################################################
#
# The Kolmogorov-Smirnov two-sided calculation based on samples of two distributions
#
#
#' KS_D
#'
#' Calculates the two-sided KS statistic given vectors for x and y
#'
#' @param x a vector or matrix of real numbers
#' @param y a vector or matrix of real numbers
#'
#' @return a vector of numbers with length equal to the number of trials (N)
#'
#' @examples
#' KS_D(c(1,2,3,4,5),c(1,2,3,4,4))
#' KS_D(matrix(runif(100),ncol=7),matrix(runif(100),ncol=7))
#'
#' @export
#' KS_D()	
KS_D <- function(x,y){

	KS_D2 <- function(x,y){
	  alternative <- "two.sided"
	  x <- x[!is.na(x)]
	  n <- length(x)
	  n.x <- as.double(n)
  
	  y <- y[!is.na(y)]
	  n.y <- length(y)
  
	  w <- c(x, y)
	  z <- cumsum(ifelse(order(w) <= n.x, 1/n.x, -1/n.y))
	  z <- z[c(which(difn(sort(w)) != 0), n.x + n.y)] #exclude ties
	  STATISTIC <- max(abs(z))
#	  STATISTIC <- switch(alternative, two.sided = max(abs(z)), 
#                      greater = max(z), less = -min(z))
                      
  	return(STATISTIC)
  }
  
  M <- fitmetric_check(x,y)
  xx  <- M[[1]]
  yy  <- M[[2]]
  N   <- M[[3]]
  dof <- M[[4]]

  out <- apply(cbind(xx, yy),1,function(s){
  	s1 <- s[1:dof]
  	s2 <- s[(dof+1):(2*dof)]
  	KS_D2(s1,s2)
  	})
  
  
  return(out)
  
  }