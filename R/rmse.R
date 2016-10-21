##############################################################################################################################
#
# The rmse calculation based on two numeric vectors of equal length
#
#
#' rmse
#'
#' Calculates rmse given vectors for y and y_pred
#'
#' @param obs a vector or matrix of real numbers
#' @param mdl a vector or matrix of real numbers
#'
#' @return a vector of numbers with length equal to the number of trials (N)
#'
#' @examples
#' rmse(seq(3:20), (seq(3:20)+rnorm(18)^2))
#'
#' @export
#'rmse
rmse <- function(obs, mdl){
	#need to know if these are vectors of numbers or matrices
	#need to know that they're the same dimensions
	#need to know which dimension is dof and which is N
	#if vectors, convert to matricies

	gclasses=c("numeric", "integer")
	if(class(obs) %in% gclasses & class(mdl) %in% gclasses & length(obs)==length(mdl)){
		obs <- matrix(obs,nrow=1)
		mdl <- matrix(mdl,nrow=1)
		} else if(class(obs)=="matrix" & class(mdl)=="matrix"){
			
			
		} else {
			stop("obs and mdl classes or lengths different")
		}


	if(class(obs)     == "matrix" & 
	   class(mdl)     == "matrix"  &
	   dim(obs)[[1]]  == dim(mdl)[[1]]  & 
	   dim(obs)[[2]]  == dim(mdl)[[2]]
	   ){		
		N=dim(obs)[[1]]
		dof=dim(obs)[[2]]
		}

	#get delta
	ed 		<- obs-mdl								#predicted ep									

	#calculate RMSE numerator
	n1		<- ed*ed							#square the individual elements (not matrix multiplication)
	n1s		<- rowSums(n1)						#sum the individual rows
	num		<- sqrt(n1s)						#sq root of individual elements -> numerator

	#calculate RMSE denominator
	den		<- sqrt(dof)						#denominator

	#calculate rmse (this is an array of rmse calculations based on noise)
	rmsevector	<- num/den

	return(rmsevector)

}