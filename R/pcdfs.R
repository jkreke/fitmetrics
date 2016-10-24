##############################################################################################################################
#
# generate a data frame with all possible values (within a width of bw) of the fitmetric with corresponding 
#   probabaility density and cumulative density functions given a particular number of degrees
#	of freedom (dof) and a particular noise distribution function (dist).
#	By default, use a million samples (order=6), normal distribution with mean=0 and sd=1, and 
#   return values with 2 decimal places.
#
#
#' Construct pdf and cdf for one of several distributions and one of several possible noise distributions
#'
#' Generates the vectors of random numbers for both observation and model and calls pcdfs
#'
#' @param dof an integer
#' @param order a real number
#' @param ndecimals an integer
#' @param dist a random number distribution function
#' @param fitmetric a character string naming a standard fit metric ("R2", "rmse", or "user")
#' @param ... any argument that functions within this routine might use
#'
#' @return a data frame
#'
#' @examples
#' pcdf(5, order=5)
#' pcdf(10, order=4, fitmetric="rmse")
#'
#' @export
#' pcdfs()
pcdfs <- function(dof, order=6, ndecimals=2, dist='normal', fitmetric="R2", ... ){

if(order>=7){stop(paste("order is too large (10M) -- calculation time too long. Make order<7. Fractions OK."))}
N=round(10^order,0)
bw=1/10^ndecimals #bin width

dN <- dof*N
rnums1 <- rnum2 <- rep(0,dN)


           if(	dist=="normal")		{	rnums1 <- rnorm( n=dN, ... );         rnums2 <- rnorm(   n=dN, ... )
	} else if(	dist=="uniform")	{	rnums1 <- runif( n=dN, ... );         rnums2 <- runif(   n=dN, ... )
	} else if(	dist=="lognormal")	{	rnums1 <- rlnorm(n=dN, ... );         rnums2 <- rlnorm(  n=dN, ... )
	} else if(	dist=="chisq")		{	rnums1 <- rchisq(n=dN, df=dof, ... ); rnums2 <- rchisq(  n=dN, df=dof, ... )
		
	} else if(	dist=="poisson")	{	rnums1 <- rpois( n=dN, ... );         rnums2 <- rpois(   n=dN, ... )
	} else if(	dist=="binomial")	{	rnums1 <- rbinom(n=dN, ... );         rnums2 <- rbinom(  n=dN, ... )
	}



if(fitmetric=="R2")		{fitdf <- gen_R2df(   dof, N, bw, rnums1, rnums2)}
if(fitmetric=="rmse")	{fitdf <- gen_rmsedf( dof, N, bw, rnums1, rnums2)}
if(fitmetric=="user")	{fitdf <- gen_userdf( dof, N, bw, rnums1, rnums2)}

return(fitdf)
}