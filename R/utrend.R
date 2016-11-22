##############################################################################################################################
#
# find the general trend (Positive or Negative) of the user function
#
#
#' Determine the general trend (general, overall slope) of the fitmetric function
#'
#' Calculates the fitmetric function at two distant points and calculates the slope using lineal regression.
#'
#' @param fun as function R2, rmse, or user (user-defined function)
#'
#' @return a character string ("Increasing", "Decreasing", "Flat", "Uncertain")
#'
#' @examples
#' utrend(R2)
#' utrend(rmse)
#'
#' @export
#' utrend()
utrend <- function(fun){

	#initiate varaiables
	nsamples <- 3*10^5
	xA  <- 3
	xB  <- 1000
	fmt <- "%1.2f"

	#generate two matricies at two distant points to determine general function trend
	#use rnorm with default paramters
	#pointA
	matA <- matrix(rnorm(nsamples), ncol=xA)
	matB <- matrix(rnorm(nsamples), ncol=xA)
	vA <- fun(matA, matB)
	vA <- sum(vA)/length(vA)
	vA <- sprintf(fmt,vA)
	
	#pointB
	matA <- matrix(rnorm(nsamples), ncol=xB)
	matB <- matrix(rnorm(nsamples), ncol=xB)
	vB <- fun(matA, matB)
	vB <- sum(vB)/length(vB)
	vB <- sprintf(fmt, vB)

	df <- data.frame(v=c(vA,vB), x=c(xA,xB))
	mdl <- lm(v~x,data=df)
	slope <- mdl$coefficient[[2]]
	
		   if(slope> 0)	{trend <- "Increasing"
	} else if(slope==0)	{trend <- "Flat"
	} else if(slope< 0)	{trend <- "Decreasing"
	} else 				{trend <- "Uncertain"
		}


return(trend)

}