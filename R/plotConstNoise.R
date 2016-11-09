##############################################################################################################################
#
# Plot the fitvals for a range of dofs that are equivalent to a single measured fitval
#		Plot the measured fit value (green asterisk)
#		Plot the noise level (noise threshold)) (red)
#		Shade the area shoing improved fit measures
#		Plot the fit equivalent curve (black)
#		if desired, plot the noise level that equals fitval
#
#
#' Plot FitValue Constant Noise
#'
#' Plots the Fit Equivalent
#'
#' @param measured_value a real number within the range of fitmetric
#' @param dof an integer
#' @param pct a real number between 0 and 1, minimum acceptable level of noise
#' @param order a real number
#' @param plot_pctr2 a logical value indicating whether to include or not the fitEquive placed at each dof.
#' @param fitmetric a character string naming a standard fit metric (R2, rmse, or user)
#' @param ... any argument that functions within this routine might use
#'
#' @return ggplot object
#'
#' @examples
#' plotConstNoise(0.8, 5)
#' plotConstNoise(0.1, 5, fitmetric=rmse)
#'
#' @export
#' plotConstNoise()
plotConstNoise <- function(measured_value, dof, pct=0.95, order=4, plot_pctr2=F, fitmetric=R2, ...){

	fitmetric.character <- deparse(substitute(fitmetric))
	fitmetric_trend <- utrend(fitmetric)
	fitval			<- measured_value
	mcolor 			<- c("red", "blue", "forestgreen", "slategray4", "gray20", "black")
	

	# get the pcdf for this dof
	dfx <- pcdfs(dof=dof, order=order, fitmetric=fitmetric, ...)
	
	#if(dof<30){doflist = c(2:30)} else {lo=dof-15;hi=dof+15;doflist=c(lo:hi)}
	doflist <- c(2:30)
	pctlist <- c(pct)
	
	
	# this will add a plot of points that follow the curve where the pct equals measured_value(aka fitval) -- just to show the probability of noise for this R2.

	# using that dfx, find the closest dfx$R2 (aka pct) to the given fitval, or pct_r2
	if(plot_pctr2){
		pct_fv <- dfx$cdf[dfx$fitval>=fitval][1]
		pctlist=c(pctlist,pct_fv)			#take out if not plotting pct_R2.  Comprising pct and pct_R2
		}

	doflength = length(doflist)
	pctlength = length(pctlist)

	ptable <- NoiseTable(doflist=doflist, pctlist=pctlist, order=order, fitmetric=fitmetric, trend=fitmetric_trend, ...)  #ptable is the list of noiselevel values for this fitmetric at each dof
	noiselevel <- ptable[(dof-1),1]			#column 1 is the noiselevel values

	if(fitmetric.character=="R2"){	ylb=expression(R^2);  gtitle="R-squared"}
	if(fitmetric.character=="rmse"){ylb=expression(RMSE); gtitle="RMSE"}
	if(fitmetric.character=="user"){ylb=expression(user); gtitle="user"}

	f = fitEquiv(fitval,dof,pct,fitmetric=fitmetric, trend=fitmetric_trend, ...)
	               if(fitmetric_trend=="Decreasing"){
				          ptable$fitEquiv <- f*(1-ptable[,1]) + ptable[,1]
			} else if(fitmetric_trend=="Increasing") {
				          ptable$fitEquiv <- f*(ptable[,1])
			} else {
				          stop("fitmetric trend flat or uncertain")
			}		
	ptable$fitEquiv[(dof-1)]=measured_value   #fitEquiv at dof must equal the measured_value at dof.  ptable will be a little off,due to randomness so make them equal.


	tx = max(doflist[doflength])
	ttx = 9/16*tx
	if(length(ptable)==3){ptable <- ptable[c(1,3,2)]}	#ensure proper order of columns;  if pct_r2=T, then swap 2<->3 to keep R2Equiv in pos 2


	bline <- floor(2/3*nrow(ptable))
	hib <- max(ptable[bline,1], ptable[bline,2])
	lob <- min(ptable[bline,1], ptable[bline,2])
	
	bscale=max(ptable[,1],ptable[,2]) - min(ptable[,1],ptable[,2])
	tv=0.9
	if(hib<0.6*bscale){tv=0.95}
	if(lob>0.3*bscale){tv=0.38}
	if((hib-lob)*bscale>0.5){tv=hib-0.05}
	
	
	tval <- tv-seq(0,0.4,by=0.05)*bscale

	
	plt <- ggplot(ptable) +
			geom_point(data=data.frame(fitval,dof), aes(dof,fitval), shape=8, color=mcolor[3],size=5,na.rm=T) + 
			geom_point(aes(as.numeric(row.names(ptable)),ptable[,2]),shape=16,color=mcolor[6],size=2,na.rm=T) +
			geom_point(data=data.frame(noiselevel, dof),   aes(dof,noiselevel),    shape=16,color=mcolor[1],size=3,na.rm=T) +
			geom_point(aes(as.numeric(row.names(ptable)),ptable[,1]),shape=1, color=mcolor[1],size=2,na.rm=T) +
			ggtitle(paste(gtitle, "Noise Baseline and Equivalent Measure \nwith Constant Noise Level")) +
			xlab("Degrees of Freedom") +
			ylab(ylb) + 
			geom_text(x=ttx, y=tval[1], label=paste0("dof = ",dof,", measured value = ",fitval), 	color=mcolor[3], hjust=0, size=4) +
			geom_text(x=ttx, y=tval[2], label=paste0("fitEquiv = ", f), 							color=mcolor[6], hjust=0, size=4) +
			geom_text(x=ttx, y=tval[3], label=paste0("fitNoise = ",noiselevel), 					color=mcolor[1], hjust=0, size=4) + 
			geom_text(x=ttx, y=tval[4], label=paste0("percentile = ",100*pctlist[1],"%"), 			color=mcolor[1], hjust=0, size=4) +
			geom_text(x=ttx, y=tval[5], label=paste0("Improved Measure (light area)"), 				color=mcolor[4], hjust=0, size=4) +
			
			geom_point(data=data.frame(x=ttx-0.5, y=tval[1]), aes(x,y), shape=8,  color=mcolor[3], size=5) + 
			geom_point(data=data.frame(x=ttx-0.5, y=tval[2]), aes(x,y), shape=16, color=mcolor[6], size=2) +
			geom_point(data=data.frame(x=ttx-0.5, y=tval[3]), aes(x,y), shape=16, color=mcolor[1], size=3) +
			geom_point(data=data.frame(x=ttx-0.5, y=tval[4]), aes(x,y), shape=1,  color=mcolor[1], size=2)
			
			

	if(fitmetric_trend=="Decreasing"){plt <- plt +
			geom_ribbon(aes(x=as.numeric(row.names(ptable)), ymin=ptable[,2], ymax=1),fill=mcolor[4],alpha=0.3,na.rm=T)}		#ymax here might be dependent on the metric				
	if(fitmetric_trend=="Increasing"){plt <- plt +
			geom_ribbon(aes(x=as.numeric(row.names(ptable)), ymax=ptable[,2], ymin=0),fill=mcolor[4],alpha=0.3,na.rm=T)}
			

	#if pct_r2 is T, plot the noise level where pct=fitval
	if(plot_pctr2){plt <- plt + 
			geom_point(aes(as.numeric(row.names(ptable)),ptable[,3]),color=mcolor[2],size=2) +
			geom_text(x=ttx, y=tval[7], label=paste0(fitmetric," Noise Percentile = ",pctlist[2]), color=mcolor[2], hjust=0,size=4,na.rm=T)}

	#if fitval is in the noise, show the improved but still noisy R2 values in a black ribbon.
	if(fitval<ptable[dof,1] & fitmetric_trend=="Decreasing"){ plt <- plt +
			geom_ribbon(aes(x=as.numeric(row.names(ptable)), ymin=ptable[,2], ymax=ptable[,1]),fill=mcolor[5],alpha=0.7,na.rm=T) +
			geom_text(x=ttx, y=tval[6], label=paste0("Unacceptable Noise (dark area)"), color=mcolor[5], hjust=0, size=4) +
			geom_point(data=data.frame(fitval,dof), aes(dof,fitval),size=4,shape=8, color=mcolor[3],na.rm=T) }			
			
	#if fitval is in the noise, show the improved but still noisy RMSE values in a black ribbon.
	if(fitval>ptable[dof,1] & fitmetric_trend=="Increasing"){ plt <- plt +
			geom_ribbon(aes(x=as.numeric(row.names(ptable)), ymin=ptable[,1], ymax=ptable[,2]),fill=mcolor[5],alpha=0.7,na.rm=T) +
			geom_text(x=ttx, y=tval[6], label=paste0("Unacceptable Noise (dark area)"), color=mcolor[5], hjust=0, size=4) +
			geom_point(data=data.frame(fitval,dof), aes(dof,fitval),size=4,shape=8, color=mcolor[3],na.rm=T) }				


	return(plt)
}