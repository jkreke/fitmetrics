##############################################################################################################################
# Plot Fit Equivalent for a single fitmetric value across a range of dofs and a constant measured value
#
#' Plot Fit Equivalent and Constant Value
#'
#' Plots the Fit Equivalent with Constant Measured Value
#'
#' @param measured_value a real number, the actual measured value
#' @param doflist a vector of integers for the degrees of freedom
#' @param pct a real number between 0 and 1, acceptable minimum level of noise
#' @param order a real number
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric (R2, rmse, or user)
#' @param ... any argument that functions within this routine might use
#'
#' @return ggplot object
#'
#' @examples
#' plotConstValue(0.8, doflist=c(2:10),order=3)
#' plotConstValue(0.1, doflist=c(2:10),order=3,fitmetric=rmse)
#'
#' @export
#' plotConstValue()
plotConstValue <- function(measured_value, doflist=c(2:30), pct=0.95, order=4, ndecimals=2, fitmetric=R2, ...){

	fitval=measured_value
	fitmetric_trend <- utrend(fitmetric)
	if(fitmetric_trend=="Increasing"){borc <- "Ceiling"}
	if(fitmetric_trend=="Decreasing"){borc <- "Baseline"}


	pct <- pct[1]										#ensure only one pct is used
	dfx <- Table_dofbypct(doflist=doflist, pctlist=pct, order=order, ndecimals=ndecimals, fitmetric=fitmetric, trend=fitmetric_trend,...)

	dfx$fitEquiv <- NA

	n <- nrow(dfx)

	for(i in 2:n){dfx$fitEquiv[i] <- fitEquiv(fitval, dof=as.numeric(row.names(dfx)[i]), pct=pct, ndecimals=ndecimals, order=order, fitmetric=fitmetric, trend=fitmetric_trend,...)}
	fitmetric.character <- deparse(substitute(fitmetric))
	gtitle <- fitmetric.character
	ylb <- fitmetric.character
	#if(fitmetric.character=="R2"){	gtitle = "R-squared"; 	ylb <- expression(R^2)}
	#if(fitmetric.character=="rmse"){gtitle = "RMSE"; 		ylb <- expression(RMSE)}
	#if(fitmetric.character=="user"){gtitle = "user"; 		ylb <- expression(User)}
	maxx		<- max(doflist)	
	mxy			<- max(dfx[,1],dfx$fitEquiv,fitval)
	miny		<- min(dfx[,1],dfx$fitEquiv,fitval)
	if(fitmetric_trend=="Decreasing"){miny=0}
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
		ggtitle(paste(gtitle, "Noise",borc,"and Equivalent Measure\nwith Constant Measured Value")) +
		annotate("text", x=maxx, y=(pcy-0.05),		label=paste0(borc," Noise Level\npercentile = ",pct*100,"%"), 	color='red',  hjust=1) + 
		annotate("text", x=maxx, y=(eqy-0.05),		label=paste0("Equivalent Value"), 				color='blue', hjust=1) +
		annotate("text", x=maxx, y=(fitval+0.05),	label=paste0("Measured Value = ",fitval), 		color='black',hjust=1)
	
	
	return(plot)
	
}