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
pcdfs <- function(dof, order=6, ndecimals=2, dist='normal', fitmetric=R2, ... ){

if(order>=7){stop(paste("order is too large (10M) -- calculation time too long. Make order<7. Fractions OK."))}
N=round(10^order,0)
bw=1/10^ndecimals #bin width

dN <- dof*N
mdl <- obs <- rep(0,dN)


           if(	dist=="normal")     {	mdl <- rnorm( n=dN, ... );         obs <- rnorm(   n=dN, ... )
	} else if(	dist=="uniform")    {	mdl <- runif( n=dN, ... );         obs <- runif(   n=dN, ... )
	} else if(	dist=="lognormal")  {	mdl <- rlnorm(n=dN, ... );         obs <- rlnorm(  n=dN, ... )
	} else if(	dist=="chisq")      {	mdl <- rchisq(n=dN, df=dof, ... ); obs <- rchisq(  n=dN, df=dof, ... )
		
	} else if(	dist=="poisson")    {	mdl <- rpois( n=dN, ... );         obs <- rpois(   n=dN, ... )
	} else if(	dist=="binomial")   {	mdl <- rbinom(n=dN, ... );         obs <- rbinom(  n=dN, ... )
	}


#establish matricies -- make matricies of N rows by dof columns
mdl		<- matrix(mdl, nrow=N, ncol=dof)
obs  	<- matrix(obs, nrow=N, ncol=dof)	

#call the fitmetric
metc    <- fitmetric(mdl,obs)

#separate metrics into bins of width bw (histogram metic) to get pdf.  sum pdf to get cdf.
br		<- seq(c(min(metc)), c(max(metc)+bw), by=bw)
met		<- br[2:length(br)]								#metric values (bins)
meth	<- hist(metc, breaks=br, plot=F)				#histogrammed
pdf 	<- meth$counts/sum(meth$counts)					#prob density
cdf 	<- cumsum(pdf)									#cumulative probability density
fdf 	<- data.frame(fitval=met, pdf, cdf)


return(fdf)
}