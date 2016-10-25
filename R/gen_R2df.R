##############################################################################################################################
#
# generate the pdf and cdf for the R2 function
#
#
#' Generate R-squared data frame of pdf and cdf values
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
#' gen_R2df()
gen_R2df <- function(dof, N, bw, obs, mdl){

#establish matricies
x		<- matrix(mdl, nrow=N, ncol=dof)
y  		<- matrix(obs, nrow=N, ncol=dof)	#make a matrix of N rows by n columns

#Call R2 function
R2c <- R2(x,y)

#separate R2s into bins of width bw (histogram R2) to get pdf.  sum pdf to get cdf.
br		<- seq(0, 1, by=bw)
R2		<- br[2:length(br)]					#R2 values (bins)
R2h		<- hist(R2c, breaks=br, plot=F)		#histogrammed R2
pdf 	<- R2h$counts/sum(R2h$counts)		#prob density
cdf 	<- cumsum(pdf)						#cumulative probability density
fdf 	<- data.frame(fitval=R2, pdf, cdf)

return(fdf)
}