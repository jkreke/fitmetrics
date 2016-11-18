##############################################################################################################################
#
# get threshold percentiles
#
#
#' Print boilerplate stats
#'
#' Prints a standard set of stats for a given degree of freedom
#'
#' @param measured_value a real number within the range of fitmetric
#' @param dof  an integer
#' @param pct  a real number between 0 and 1
#' @param fitmetric a character string
#' @param order a real number
#' @param ndecimals an integer
#' @param dist a random number distribution function
#' @param table a logical value
#' @param ... any argument that functions within this routine might use
#'
#' @return data frame
#'
#' @examples
#' fit(6, dof=10, pct=0.99)
#'
#' @export
#' fit()
#
fit <- function(measured_value, dof, pct=0.95, fitmetric=R2, order=5, ndecimals=2, dist=rnorm, table=TRUE, ... ){
	dfx <- pcdfs(dof,fitmetric=fitmetric,order=order,dist=dist,...)
	
	fmt <- paste0("%1.",ndecimals,"f")
	ftrend <- utrend(fitmetric)
	if(ftrend=="Decreasing"){
		current_percentile <- dfx$cdf[dfx$fitval>=measured_value][1]  #list all cdfs where fitval>=measured and take first one in the list
		borc <- "Noise Baseline:"
		}
	if(ftrend=="Increasing"){
		current_percentile <- 1-dfx$cdf[dfx$fitval>=measured_value][1]
		borc <- "Noise Ceiling:"
		}

	if(table){
		tfitmetric 	<- "Fit Metric:"
		vfitmetric 	<- deparse(substitute(fitmetric))
	
		tdof		<- "Degrees of Freedom:"
		vdof		<- dof
	
		tdist 		<- "Noise Distribution:"
		vdist 		<- deparse(substitute(dist))
	
		tnsamples 	<- "Number of Samples:"
		vnsamples 	<- 10^order
		
		tmv			<- "Measured Value:"
		vmv			<- measured_value
	
		tnlevel 	<- borc
		nlevel 		<- fitNoise(dof=dof, pct=pct, ndecimals=ndecimals, fitmetric=fitmetric, dist=dist, order=order, trend=ftrend, ...)
		vnlevel 		<- sprintf(fmt, nlevel)
	
		tdpct 		<- "Min Acceptable Noise Percentile:"
		vdpct 		<- sprintf(fmt, pct)
	
		tapct 		<- "Calculated Noise Percentile:"
		vapct 		<- sprintf(fmt, current_percentile)

		tfiteq 		<- "Fit Equivalent Value:"
		feq    		<- fitEquiv(measured_value=measured_value, dof=dof, pct=pct, ndecimals=ndecimals, fitmetric=fitmetric, dist=dist, order=order, trend=ftrend,...)
		vfiteq 		<- sprintf(fmt, feq)
	
	
		outdf 		<- data.frame(Parameter	=c(tfitmetric,tdof,tdist,tnsamples,tmv,tnlevel,tdpct,tapct,tfiteq), 
									Value	=c(vfitmetric,vdof,vdist,vnsamples,vmv,vnlevel,vdpct,vapct,vfiteq)
							)
	
		}
	
	if(table){return(outdf)} else {return(cat(sprintf(fmt,current_percentile)))}
	
}