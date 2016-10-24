##############################################################################################################################
#
# Plot Equiv for a single fitmetric value across a range of dofs
#
#' Plot Fit Equivalent
#'
#' Plots the Fit Equivalent
#'
#' @param measured_value a real number, the actual measured value
#' @param doflist a vector of integers for the degrees of freedom
#' @param pct a real number between 0 and 1, acceptable minimum level of noise
#' @param order a real number
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric ("R2", "rmse", or "user")
#' @param ... any argument that functions within this routine might use
#'
#' @return ggplot object
#'
#' @examples
#' plotEquiv(0.8)
#' plotEquiv(0.1, fitmetric="rmse")
#'
#' @export
#' plotConstValue()
plotConstValue <- function(measured_value, doflist=c(2:30), pct=0.95, order=4, ndecimals=2, fitmetric='R2', ...){

	fitval=measured_value
	fitmetric_trend <- utrend(fitmetric)

	pct <- pct[1]										#ensure only one pct is used
	dfx <- NoiseTable(doflist=doflist, pctlist=pct, order=order, ndecimals=ndecimals, fitmetric=fitmetric, trend=fitmetric_trend,...)

	dfx$fitEquiv <- NA

	n <- nrow(dfx)

	for(i in 1:n){dfx$fitEquiv[i] <- fitEquiv(fitval, dof=as.numeric(row.names(dfx)[i]), pct=pct, ndecimals=ndecimals, order=order, fitmetric=fitmetric, trend=fitmetric_trend,...)}
	if(fitmetric=="R2"){	gtitle = "R-squared"; 	ylb <- expression(R^2)}
	if(fitmetric=="rmse"){	gtitle = "RMSE"; 		ylb <- expression(RMSE)}
	if(fitmetric=="user"){	gtitle = "user"; 		ylb <- expression(User)}
	maxx		<- max(doflist)	
	mxy			<- max(dfx[,1],dfx$fitEquiv,fitval)
	miny		<- min(dfx[,1],dfx$fitEquiv,fitval)
	if(fitmetric_trend=="Negative"){miny=0}
	else{miny		<- max(miny,-10)}
	eqy <- dfx$fitEquiv[row.names(dfx)==maxx]
	pcy	<- dfx[row.names(dfx)==maxx,1]
	plot <- ggplot(dfx) + 
		geom_point(aes(as.numeric(row.names(dfx)),dfx[,1]),color='red') + 		#column dfx[,1] is named for the pct used, which can change every time.
		geom_point(aes(as.numeric(row.names(dfx)),fitEquiv),color='blue',na.rm=T) + 
		geom_hline(aes(yintercept=fitval),color='black') + 
		ylim(miny,mxy) +
		xlab("Degrees of Freedom") +
		ylab(ylb) +
		ggtitle(paste(gtitle, "Noise Baseline and Equivalent Measure\nwith Constant Measured Value")) +
		geom_text(aes(x=maxx, y=(pcy-0.05)),		label=paste0("Baseline Noise Level\npercentile = ",pct*100,"%"), 	color='red',  hjust=1) + 
		geom_text(aes(x=maxx, y=(eqy-0.05)),		label=paste0("Equivalent Value"), 				color='blue', hjust=1) +
		geom_text(aes(x=maxx, y=(fitval+0.05)),	label=paste0("Measured Value = ",fitval), 		color='black',hjust=1)
	
	
	return(plot)
	
}