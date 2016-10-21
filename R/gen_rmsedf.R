##############################################################################################################################
#
# generate the pdf and cdf for the rmse function
#
#
#' Generate rmse data frame of pdf and cdf values
#'
#' Builds the pdf and cdf data frame based on random numbers generated from a specific noise distribution
#'
#' @param dof an integer
#' @param N an integer
#' @param bw a real number
#' @param obs a vector of random numbers dof*N long
#' @param mdl a vector of random numbers dof*N long
#'
#' @return a data frame
#'
#' @export
#' gen_rmsedf()
gen_rmsedf <- function(dof, N, bw, obs, mdl){

#establish matricies
obs <- matrix(obs,ncol=dof)
mdl <- matrix(mdl,ncol=dof)

#call rmse function
rmsec 	<- rmse(obs,mdl)


#separate rmses into bins of width bw (histogram rmse) to get pdf.  sum pdf to get cdf.
br		<- seq(0, c(max(rmsec)+bw), by=bw)	#add an extra bin in case elements fall on the upper bound
rmse	<- br[2:length(br)]					#rmse values (bins)
rmseh	<- hist(rmsec, breaks=br, plot=F)	#histogrammed rmse
pdf		<- rmseh$counts/sum(rmseh$counts)	#prob density
cdf		<- cumsum(pdf)						#cumulative probability density
fdf		<- data.frame(fitval=rmse, pdf, cdf)		#

return(fdf)
}