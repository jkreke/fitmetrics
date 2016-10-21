##############################################################################################################################
#
# The user-defined function for calculation of some fit metric based on two numeric vectors of equal length or matrices of 
# the same dimensions
#
#
#' user
#'
#' Calculates the user-defined functions given vectors/matrices for m1 and m2
#'
#' @param m1 a vector or matrix of real numbers
#' @param m1 a vector or matrix of real numbers
#'
#' @return a vector of numbers with length equal to the number of trials (N)
#'
#' @examples
#' user( seq(2:10),(3*rnorm(2:10)+4) )
#'
#' @export
#'user
	
user <- function(m1,m2){				#user function takes two matrices, each N by dof
	gclasses = c("numeric", "integer")
	if(class(m1) %in% gclasses & class(m2) %in% gclasses){
		mm1 <- matrix(m1,nrow=1)
		mm2 <- matrix(m2,nrow=1)
	} else if(class(m1)=="matrix" & class(m2)=="matrix"){
		mm1 <- m1
		mm2 <- m2
	} else {
		stop("arguments are of the wrong class")
	}

	function1=F
	function2=T

	K <- rep(NA,nrow(mm1));					#improve speed by defining the variable to store calculation
	
			   if(function1){
			for(i in 1:nrow(mm1)){
				K[i]<-cor(mm1[i,],mm2[i,])^4		#user function across each row of the matricies.  Each row is an independent sample.
				}
				
		} else if(function2){
			for(i in 1:nrow(mm1)){
				K[i]<-rmse(mm1[i,],mm2[i,])		#user function across each row of the matricies.  Each row is an independent sample.
				}
			K = K*K
		
		} else {
			
		}
		
		
	return(K)
	}