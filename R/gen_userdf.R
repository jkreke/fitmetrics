##############################################################################################################################
#
# generate the pdf and cdf for the user function
#
#
#' Generate vector of values used to build pdf and cdf
#'
#' Calculates the user function output with the frequencies of each possible outcome to width bw.
#'
#' @param dof an integer
#' @param N an integer
#' @param bw a real number describing the bin width
#' @param obs a vector of random numbers dof*N long
#' @param mdl a vector of random numbers dof*N long
#'
#' @return a vector of numbers
#'
#' @examples
#' user <- function(m1,m2){			#user function takes two matrices, each N by dof
#'	K <- rep(NA,nrow(m1));			#improve speed by defining the variable to store calculations
#'	for(i in 1:nrow(m1)){
#'		K[i]<-cor(m1[i,],m2[i,])^2};	#For comparison, this is another method of calculating R-square
#'	return(K)}
#' plotpdf(dof=3,fitmetric="user",order=5)
#' plotpdf(dof=3,fitmetric="R2",order=5) #for comparison
#'
#' @export
#' gen_userdf()
gen_userdf <- function(dof, N, bw, obs, mdl){

#establish matrices
obs <- matrix(obs, nrow=N, ncol=dof)
mdl <- matrix(mdl, nrow=N, ncol=dof)

#call user-defined function
userc <- user(obs, mdl)

#separate user measures into bins of width bw (histogram user) to get pdf.  sum pdf to get cdf.
br		<- seq(c(min(userc)), c(max(userc)+bw), by=bw)	#add an extra bin in case elements fall on the upper bound
user	<- br[2:length(br)]					#user values (bins)
userh	<- hist(userc, breaks=br, plot=F)	#histogrammed user
pdf		<- userh$counts/sum(userh$counts)	#prob density
cdf		<- cumsum(pdf)						#cumulative probability density
fdf		<- data.frame(fitval=user, pdf, cdf)		#

return(fdf)
}