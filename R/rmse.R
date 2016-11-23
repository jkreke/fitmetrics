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
#' rmse(c(2:20),c(2:20 + c(rep(0.1,5),rep(-0.2,10),rep(0.3,4))))
#' mata <- matrix(runif(10000),ncol=5)
#' matb <- matrix(runif(10000),ncol=5)
#' rmse(mata, matb)
#'
#' @export
#' rmse()
rmse <- function(obs, mdl){

	mlist <- fitmetric_check(obs,mdl)
	obs <- mlist[[1]]
	mdl <- mlist[[2]]
	N   <- mlist[[3]]
	dof <- mlist[[4]]

	#get delta
	ed 		<- obs-mdl							#predicted ep									

	#calculate RMSE numerator
	n1		<- ed*ed							#square the individual elements (not matrix multiplication)
	n1s		<- rowSums(n1)						#sum the individual rows
	num		<- sqrt(n1s)						#sq root of individual elements -> numerator

	#calculate RMSE denominator
	den		<- sqrt(dof)						#denominator

	#calculate rmse (this is an array of rmse calculations based on noise)
	out	<- num/den

	return(out)

}