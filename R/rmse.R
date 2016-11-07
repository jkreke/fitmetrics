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

	mlist <- Consistency_Check(x,y)
	x <- mlist[[1]]
	y <- mlist[[2]]
	N <- mlist[[3]]
	dof <- mlist[[4]]

	#get delta
	ed 		<- obs-mdl								#predicted ep									

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