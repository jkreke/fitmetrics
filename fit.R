##############################################################################################################################
#
# The R-squared calculation based on two numeric vectors of equal length
#
#
#' R-squared
#'
#' Calculates R-squared given vectors for x and y
#'
#' @param x a vector of real numbers
#' @param y a vector of real numbers
#'
#' @return a data frame
#'
#' @examples
#' R2( seq(2:10),(3*seq(2:10)+4) )
#'
#' @export
#'R2
R2 <- function(x, y){
	if(length(x)!=length(y)){stop("r.sq measure: length of x must equal length of y")}
	xh <- x-mean(x)
	yh <- y-mean(y)
	
	num <- sum(xh*yh)^2
	den <- sum(xh^2)*sum(yh^2)
	
	R2 <- num/den
	return(R2)
}


##############################################################################################################################
#
# simple function to capitalize first letters of words for use in titles
#
#
#' Capitalize a string
#'
#' Capitalizes each word in a character string
#'
#' @param x a character string
#'
#' @return a character string
#'
#' @examples
#' cap1("this is a character string")
#'
#' @export
#' cap1()
cap1 <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s,1,1)), substring(s, 2),
          sep="", collapse=" ")
}




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
#' @rnum a vector of random numbers dof*N long
#' @pnum a vector of random numbers dof*N long
#'
#' @return a data frame
#'
#' @export
#' gen_R2df()
gen_R2df <- function(dof, N, bw, rnums, pnums){


#initialize basic variables
R2c 	<- rep(0,N)

#define R2 components x and y (y is just e, residuals, random numbers)				
x  		<- seq(1,dof)
xb 		<- mean(x)
xd 		<- x-xb
xd 		<- t(matrix(rep(xd, N), nrow=dof, ncol=N))

e  		<- matrix(rnums, nrow=N, ncol=dof)	#make a matrix of N rows by n columns
eb 		<- rowSums(e)/dof					#get means for each row
ed 		<- e-eb								#get delta

#calculate R2 numerator
n1		<- xd*ed
n1s		<- rowSums(n1)
num		<- n1s*n1s							#numerator

#calculate R2 denominator
d1		<- xd^2
d2		<- ed^2
d1s		<- rowSums(d1)
d2s		<- rowSums(d2)
den		<- d1s*d2s							#denominator

#calculate R2 (this is an array of R2 calculations based on noise)
R2c		<- num/den							#R2

#separate R2s into bins of width bw (histogram R2) to get pdf.  sum pdf to get cdf.
br		<- seq(0, 1, by=bw)
R2		<- br[2:length(br)]					#R2 values (bins)
R2h		<- hist(R2c, breaks=br, plot=F)		#histogrammed R2
pdf 	<- R2h$counts/sum(R2h$counts)		#prob density
cdf 	<- cumsum(pdf)						#cumulative probability density
fdf 	<- data.frame(fitval=R2, pdf, cdf)

return(fdf)
}



##############################################################################################################################
#
# generate the pdf and cdf for the RMSE function
#
#
#' Generate RMSE data frame of pdf and cdf values
#'
#' Builds the pdf and cdf data frame based on random numbers generated from a specific noise distribution
#'
#' @param dof an integer
#' @param N an integer
#' @param bw a real number
#' @rnum a vector of random numbers dof*N long
#' @pnum a vector of random numbers dof*N long
#'
#' @return a data frame
#'
#' @export
#' gen_RMSEdf()
gen_RMSEdf <- function(dof, N, bw, rnums, pnums){


#initialize basic variables
RMSEc	<- rep(0,N)

#define RMSE components - residuals.  Both observed and predicted (model) are just random numbers				
e  		<- matrix(rnums, nrow=N, ncol=dof)	#make a matrix of N rows by dof columns (observation)
ep 		<- matrix(pnums, nrow=N, ncol=dof)	#make a matrix of N rows by dof columns (model)
ed 		<- e-ep								#predicted ep									

#calculate RMSE numerator
n1		<- ed*ed							#square the individual elements (not matrix multiplication)
n1s		<- rowSums(n1)						#sum the individual rows
n1ss	<- sqrt(n1s)						#sq root of individual elements
num		<- n1ss								#numerator

#calculate R2 denominator
den		<- sqrt(dof)						#denominator

#calculate RMSE (this is an array of RMSE calculations based on noise)
RMSEc	<- num/den							#RMSE


#separate RMSEs into bins of width bw (histogram RMSE) to get pdf.  sum pdf to get cdf.
br		<- seq(0, c(max(RMSEc)+bw), by=bw)	#add an extra bin in case elements fall on the upper bound
RMSE	<- br[2:length(br)]					#RMSE values (bins)
RMSEh	<- hist(RMSEc, breaks=br, plot=F)	#histogrammed RMSE
pdf		<- RMSEh$counts/sum(RMSEh$counts)	#prob density
cdf		<- cumsum(pdf)						#cumulative probability density
fdf		<- data.frame(fitval=RMSE, pdf, cdf)		#

return(fdf)
}




##############################################################################################################################
#
# generate a data frame with possible values of R2 going from 0 to 1 with corresponding 
#   probabaility density and cumulative density functions given a particular number of degrees
#	of freedom (dof) and a particular noise distribution function (dist).
#	By default, use a million samples (order=6), normal distribution with mean=0 and sd=1, and 
#   return values with 3 decimal places.
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
#' @param fitmetric a character string naming a standard fit metric ("R2" and "RMSE")
#' @param ... any argument that functions within this routine might use
#'
#' @return a data frame
#'
#' @examples
#' pcdf(5, order=5)
#' pcdf(10, order=4, fitmetric="RMSE")
#'
#' @export
#' pcdfs()
pcdfs <- function(dof, order=6, ndecimals=3, dist='normal', fitmetric="R2", ... ){

if(order>=7){stop(paste("order is too large (10M) -- calculation time too long. Make order<7. Fractions OK."))}
N=round(10^order,0)
bw=1/10^ndecimals #bin width

dN <- dof*N
rnums <- pnums <- rep(0,dN)


if(				dist=="normal")		{	rnums <- rnorm( 	n=dN, ... ); pnums <- rnorm(   n=dN, ... )
	} else if(	dist=="uniform")		{	rnums <- runif( 	n=dN, ... ); pnums <- runif(   n=dN, ... )
	} else if(	dist=="lognormal")	{	rnums <- rlnorm(	n=dN, ... ); pnums <- rlnorm(  n=dN, ... )
	} else if(	dist=="poisson")		{	rnums <- rpois(		n=dN, ... ); pnums <- rpois(   n=dN, ... )
	} else if(	dist=="binomial")	{	rnums <- rbinom(	n=dN, ... ); pnums <- rbinom(  n=dN, ... )
	}



if(fitmetric=="R2")		{fitdf <- gen_R2df(		dof, N, bw, rnums, pnums)}
if(fitmetric=="RMSE")	{fitdf <- gen_RMSEdf(	dof, N, bw, rnums, pnums)}

return(fitdf)
}


##############################################################################################################################
#
#	determine the baseline noise level (R2p, fitNoise) for a corresponding number of degrees of freedom(dof) and noise percentile(pct)
#
#
#' Find The Threshold Noise Level
#'
#' Finds the threshold noise level for a certain fit metric
#'
#' @param dof an integer
#' @param pct a real number between 0 and 1
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric ("R2" and "RMSE")
#' @param ... any argument that functions within this routine might use
#'
#' @return a real number
#'
#' @examples
#' fitNoise(7)
#' fitNoise(4, fitmetric='RMSE')
#'
#' @export
#' fitNoise()
fitNoise <- function(dof, pct=0.95, ndecimals=3, fitmetric='R2', ...){
	cdf <- pcdfs(dof, ndecimals=ndecimals, fitmetric=fitmetric, ...)[,c("fitval","cdf")]
	if(fitmetric=="RMSE"){
		c_pct <- as.numeric(1-as.numeric(pct))
		c_val <- cdf$fitval[cdf$cdf<=c_pct]
		c_val <- rev(c_val)
		nb <- c_val[1]
	} else {
		nb <- cdf$fitval[cdf$cdf>=pct][1]		#nb is the value of cdf just at the point where it's just >= p
	}
	nb <- nb + rnorm(1)*10^(-(ndecimals+2))  #add a small random number to remove any binning errors.
	nb <- round(nb,ndecimals)
	return(nb)
}


##############################################################################################################################
#
#	For a given value of R2, dof and pct, determine the noise-normalized, dof-independent, 
#      distribution-independent, R2 equivalent:  R2k
#
#' Fit Equivalent
#' 
#' Rescales the fit value to distance to the threshold
#'
#' @param fitval a real number
#' @param dof an integer
#' @param pct a real number between 0 and 1
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric ("R2" and "RMSE")
#' @param ... any argument that functions within this routine might use
#'
#' @return a real number
#'
#' @examples
#' fitEquiv(0.8, 6)
#' fitEquiv(0.1, dof=8, fitmetric="RMSE")
#'
#' @export
#' fitEquiv()
fitEquiv <- function(fitval, dof, pct=0.95, ndecimals=3, fitmetric="R2", ...){
	nlevel <- fitNoise(dof=dof, pct=pct, ndecimals=ndecimals, fitmetric=fitmetric, ...)								#find R2p
	
	if(fitmetric=="R2"){
		eqfitval <- (fitval-nlevel)/(1-nlevel + 0.00000000001)			#rescale R2 to where it lies between 1 and baseline noise; add smidge incase r2p=1
	} else if (fitmetric=="RMSE") {
		eqfitval <- (nlevel-fitval)/nlevel		
	} else {
		stop(paste(fitmetric, "is not a valid fitmetric for the fitEquiv routine"))
	}
	
	
	#make eqfitval consistent with the number of decimal places in nlevel.
	fl <- floor(nlevel)								
	nd=rep(0,length(fitval))
	if(nlevel-fl>0){
		nd <- nchar(sapply(strsplit(as.character(nlevel), ".",fixed=T), "[[", 2))} 
	eqfitval <- round(eqfitval, ndecimals)
	
	return(eqfitval)
}



##############################################################################################################################
#
# Generate a percentile analysis table listing baseline R2ps for various degrees of freedom and percentiles. 
#		Any measured R2 falling below these values (for the corresponding dof and pct)
#			1) are indistinguishable from noise and
#			2) will yield a negative R2k
#			3) should be discarded
#
#		This will run a call to R2p for each combination of dof and pct
#		A dof list of one dof (say,10), will take one pct just under a minute, each additional pct adds an equivalent amount (approx).
#		60 dof will take one pct about 5.75 min.
#
#		See plotR2Equiv for a plot of all R2 equal to a particular measure of R2 (with a certain dof and pct).
#
#' Noise Threshold Table
#'
#' Constructs a table in the form of a data frame of threshold noise values for a given fit metric, a given set of possible degrees of freedom and a given set of percentiles
#' 
#' @param dooflist
#' @param pctlist
#' @param order a real number
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric ("R2" and "RMSE")
#' @param ... any argument that functions within this routine might use
#'
#' @return a data frame
#'
#' @examples
#' NoiseTable(fitmetric="RMSE")
#'
#' @export
#' NoiseTable()
#
NoiseTable <- function(doflist=NULL, pctlist=NULL, order=4, ndecimals=2, fitmetric="R2", ...){
	if(is.null(doflist)){doflist=c(4,8,16,32,64,128)}
	if(is.null(pctlist)){pctlist=c(0.7,0.9,0.95,0.99)}
	nds <- length(doflist)	#need test here for pos integer
	nps <- length(pctlist)	#need test here for nums >0 and <1
	rownams = as.character(doflist)
	colnams = as.character(pctlist)
	
	shell <- matrix(nrow=nds, ncol=nps)
	r2ptab <- matrix(mapply(function(x,i,j) fitNoise(doflist[i], pctlist[j], order=order, ndecimals=ndecimals, fitmetric=fitmetric, ...), shell,row(shell),col(shell)), nrow=nds, ncol=nps)

	r2ptab <- as.data.frame(r2ptab)
	colnames(r2ptab) <- colnams
	rownames(r2ptab) <- rownams
	return(r2ptab)
}



##############################################################################################################################
#
# Plot the probability density function for a given number of degrees of freedom and noise distribution function
#
#
#' Plot PDF
#'
#' Plots the Probability Density Function
#'
#' @param dof  an integer
#' @param order a real number
#' @param dist a random number distribution function
#' @param fitmetric a character string naming a standard fit metric ("R2" and "RMSE")
#' @param ... any argument that functions within this routine might use
#'
#' @return ggplot object
#'
#' @examples
#' plotpdf(5, dist='uniform', fitmetric='RMSE')
#'
#' @export
#' plotpdf()
plotpdf <- function(dof, order=4, dist='normal', fitmetric='R2', ...){

df <- pcdfs(dof=dof, order=order, dist=dist, fitmetric=fitmetric, ...)
N = 10^order
dist2 <- sapply(dist, cap1)
mxy = max(df$pdf)
maxx <- max(df$fitval)
	if(fitmetric=='R2'){fmet   <- expression(R^2)}
	if(fitmetric=='RMSE'){fmet <- expression(RMSE)}
plot <- ggplot(df) + 
		geom_point(aes(fitval, pdf),size=1) +
		ylim(0,mxy) +
		xlab(fmet) + 
		ylab("Probability Density") +
		ggtitle(paste(fitmetric, "Probability Density Function")) +
		geom_text(aes(x=0.95*maxx,y=0.9*mxy,label=paste("Noise Distribution:",dist2,
													"\nDegrees of Freedom:",dof,
													"\nNumber of  Samples:",floor(N))),size=3,hjust=1)


return(plot)
}


##############################################################################################################################
#
# Plot the cumulative probability density function (cdf) for a given number of degrees of freedom and noise distribution function
#
#
#' Plot CDF
#'
#' Plots the Cumulative Probability Density Function
#'
#' @param dof an integer
#' @param order a real number
#' @param dist a random number distribution function
#' @param fitmetric a character string naming a standard fit metric ("R2" and "RMSE")
#' @param ... any argument that functions within this routine might use
#'
#' @return ggplot object
#'
#' @examples
#' plotcdf(5, dist='uniform', fitmetric='RMSE')
#'
#' @export
#' plotcdf()
plotcdf <- function(dof, order=4, dist='normal', fitmetric='R2', ...){  		#need to explicitly state distribiution here in order to get it into the plot title

r2cdf <- pcdfs(dof=dof, order=order, dist=dist, fitmetric=fitmetric, ...)
cdf <- NULL													#see http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when.  Need this to eliminate a note during R CMD check
N = 10^order
dist2 <- sapply(dist, cap1)
	if(fitmetric=='R2'){fmet   <- expression(R^2)}
	if(fitmetric=='RMSE'){fmet <- expression(RMSE)}
mxy <- max(r2cdf$cdf)
maxx <- max(r2cdf$fitval)
plot <- ggplot(r2cdf) + 
		geom_point(aes(fitval, cdf),size=1) +
		ylim(0,mxy) + 
		xlab(fmet) + 
		ylab("Cumulative Probability") +
		ggtitle(paste(fitmetric,"Cumulative Probability Density Function")) +
		geom_text(aes(x=0.95*maxx,y=0.3*mxy,label=paste("Noise Distribution:",dist2,
													"\nDegrees of Freedom:",dof,
													"\nNumber of  Samples:",floor(N))),size=3,hjust=1)

return(plot)
}


##############################################################################################################################
#
# Plot R2p for a list of percentiles (no more than 5 in the list)
#
#
#' Plot Noise threshold
#'
#' Plots the Noise threshold for each degree of freedom
#'
#' @param doflist
#' @param pctlist
#' @param order a real number
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric ("R2" and "RMSE")
#' @param ... any argument that functions within this routine might use
#'
#' @return ggplot object
#'
#' @examples
#' plotNoise()
#'
#' @export
#' plotNoise()
plotNoise <- function(doflist=c(2:30), pctlist=c(0.95), order=4, ndecimals=3, fitmetric='R2', ...){
	if(length(pctlist)>5){stop(paste("Too many percentiles to calculate", length(pctlist)))}

	doflist <- doflist[doflist>1] 
	doflim <- min(30, length(doflist))
	doflist <- doflist[1:doflim]
	
	pctlim <- min(5,length(pctlist))
	pctlist <- pctlist[1:pctlim]
	pctlist <- formatC(as.numeric(pctlist),width=(ndecimals+1),format='f',digits=ndecimals,flag='0')
	doflength <- length(doflist)
	pctlength <- length(pctlist)
	
	mcolor <- c("black", "blue", "red", "green", "darkgreen")
	sizes <- c(3.6, 3.2, 2.8, 2.4, 2.0)/2  #make the points of the first plots larger so they can be seen
	#sizes <- c(1, 0.8, 0.6, 0.4, 0.2)   #use these for geom_path instead of geom_point
	
	r2pdf <- NoiseTable(doflist=doflist, pctlist=pctlist, order=order, fitmetric=fitmetric, ...)
	mxy <- 0.9*max(r2pdf[,1])
	N = 10^order
	plt <- ggplot(r2pdf)

	maintitle <- "Baseline Noise Level\nfor Various Noise Percentiles (p)"
	if(fitmetric=='R2'){fmet   <- expression(R^2)}
	if(fitmetric=='RMSE'){fmet <- expression(RMSE)}
	gtitle <- paste(fmet,maintitle)

	if(pctlength>=1){plt <- plt + 
		geom_point(aes(as.numeric(row.names(r2pdf)), r2pdf[,1]), color=mcolor[1], size=sizes[1])  +
		geom_text(aes(x=max(doflist), y=mxy-0.00, label=paste0("p = ",pctlist[1])), color=mcolor[1], hjust=1, size=4)}
	if(pctlength>=2){plt <- plt + 
		geom_point(aes(as.numeric(row.names(r2pdf)), r2pdf[,2]), color=mcolor[2], size=sizes[2])  +
		geom_text(aes(x=max(doflist), y=mxy-0.05, label=paste0("p = ",pctlist[2])), color=mcolor[2], hjust=1, size=4)}
	if(pctlength>=3){plt <- plt + 
		geom_point(aes(as.numeric(row.names(r2pdf)), r2pdf[,3]), color=mcolor[3], size=sizes[3])  +
		geom_text(aes(x=max(doflist), y=mxy-0.10, label=paste0("p = ",pctlist[3])), color=mcolor[3], hjust=1, size=4)}
	if(pctlength>=4){plt <- plt + 
		geom_point(aes(as.numeric(row.names(r2pdf)), r2pdf[,4]), color=mcolor[4], size=sizes[4])  +
		geom_text(aes(x=max(doflist), y=mxy-0.15, label=paste0("p = ",pctlist[4])), color=mcolor[4], hjust=1, size=4)} 
	if(pctlength>=5){plt <- plt + 
		geom_point(aes(as.numeric(row.names(r2pdf)), r2pdf[,5]), color=mcolor[5], size=sizes[5])  +
		geom_text(aes(x=max(doflist), y=mxy-0.20, label=paste0("p = ",pctlist[5])), color=mcolor[5], hjust=1, size=4)}
	
	plt <- plt + 
	ggtitle(gtitle) +
	xlab("Degrees of Freedom") +
	ylab(fmet) +
	geom_text(aes(x=max(doflist), y=mxy-0.25, label=paste0("Number of Samples:",N)), color='black', hjust=1, size=3)

	
	return(plt)
}




##############################################################################################################################
#
# Plot R2k for a single R2 across a range of dofs
#
#' Plot Fit Equivalent
#'
#' Plots the Fit Equivalent
#'
#' @param fitval
#' @param doflist
#' @param pct
#' @param order a real number
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric ("R2" and "RMSE")
#' @param ... any argument that functions within this routine might use
#'
#' @return ggplot object
#'
#' @examples
#' plotEquiv(0.8)
#' plotEquiv(0.1, fitmetric="RMSE")
#'
#' @export
#' plotConstValue()
plotConstValue <- function(fitval, doflist=c(2:30), pct=0.95, order=4, ndecimals=3, fitmetric='R2', ...){

	pct <- pct[1]										#ensure only one pct is used
	df <- NoiseTable(doflist=doflist, pctlist=pct, order=order, ndecimals=ndecimals, fitmetric=fitmetric, ...)

	df$fitEquiv <- NA

	n <- nrow(df)

	for(i in 1:n){df$fitEquiv[i] <- fitEquiv(fitval, dof=as.numeric(row.names(df)[i]), pct=pct, ndecimals=ndecimals, order=order, fitmetric=fitmetric, ...)}
	if(fitmetric=="R2"){ylb=expression(R^2)}
	if(fitmetric=="RMSE"){ylb=expression(RMSE)}
	maxx		<- max(doflist)	
	mxy		<- max(df[,1],df$fitEquiv,fitval)
	miny		<- min(df[,1],df$fitEquiv,fitval)
	if(fitmetric=="R2"){miny=0}
	else{miny		<- max(miny,-10)}
	eqy <- df$fitEquiv[row.names(df)==maxx]
	pcy	<- df[row.names(df)==maxx,1]
	plot <- ggplot(df) + 
		geom_point(aes(as.numeric(row.names(df)),df[,1]),color='red') + 		#column df[,1] is named for the pct used, which can change every time.
		geom_point(aes(as.numeric(row.names(df)),fitEquiv),color='blue',na.rm=T) + 
		geom_hline(aes(yintercept=fitval),color='black') + 
		ylim(miny,mxy) +
		xlab("Degrees of Freedom") +
		ylab(ylb) +
		ggtitle(paste(fitmetric, "Noise Baseline and Equivalent Measure \nwith Constant Measured Value")) +
		geom_text(aes(x=maxx, y=(pcy-0.05)),		label=paste0("Baseline Noise Level\npercentile = ",pct*100), 	color='red',  hjust=1) + 
		geom_text(aes(x=maxx, y=(eqy-0.05)),		label=paste0("Equivalent Value"), 				color='blue', hjust=1) +
		geom_text(aes(x=maxx, y=(fitval+0.05)),	label=paste0("Measured Value = ",fitval), 		color='black',hjust=1)
	
	return(plot)
	
}

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
#' @param fitval
#' @param dof an integer
#' @param pct
#' @param order a real number
#' @param plot_pctr2 
#' @param fitmetric a character string naming a standard fit metric ("R2" and "RMSE")
#' @param ... any argument that functions within this routine might use
#'
#' @return ggplot object
#'
#' @examples
#' plotFitEquiv(0.8, 5)
#' plotFitEquiv(0.1, 5, fitmetric="RMSE")
#'
#' @export
#' plotConstNoise()
plotConstNoise <- function(fitval, dof, pct=0.95, order=4, plot_pctr2=F, fitmetric='R2', ...){

	mcolor <- c("red", "blue", "forestgreen", "slategray4", "gray20", "black")


	# get the pcdf for this dof
	df <- pcdfs(dof=dof, order=order, fitmetric=fitmetric, ...)
	
	doflist = c(2:30)
	pctlist = c(pct)
	
	
	# this will add a plot of points that follow the curve where the pct equals fitval -- just to show the probability of noise for this R2.

	# using that df, find the closest df$R2 (aka pct) to the given fitvalue, or pct_r2
	if(plot_pctr2){
		pct_fv <- df$cdf[df$fitval>=fitval][1]
		pctlist=c(pctlist,pct_fv)			#take out if not plotting pct_R2.  Comprising pct and pct_R2
		}

	doflength = length(doflist)
	pctlength = length(pctlist)

	ptable <- NoiseTable(doflist=doflist,pctlist=pctlist,order=order,fitmetric=fitmetric,...)
	#ppp <- ptable
	r2p <- ptable[(dof-1),1]
	r2k <- fitEquiv(fitval,dof=dof,...)
	if(fitmetric=="R2"){	f = (fitval-r2p)/(1-r2p);	ylb=expression(R^2);		ptable$fitEquiv <- f*(1-ptable[,1]) + ptable[,1]}
	if(fitmetric=="RMSE"){	f = (r2p-fitval)/r2p;		ylb=expression(RMSE);	ptable$fitEquiv <- ptable[,1] - f*(ptable[,1])}
	#ptable$fitEquiv <- f*(1-ptable[,1]) + ptable[,1]   	#this should be column 3 if pctr2 is being plotted and 2 if not

	tx = max(doflist[doflength])

	if(length(ptable)==3){ptable <- ptable[c(1,3,2)]}	#ensure proper order of columns;  if pct_r2=T, then swap 2<->3 to keep R2Equiv in pos 2

	
	plt <- ggplot(ptable) +
			geom_point(aes(as.numeric(row.names(ptable)),ptable[,1]),color=mcolor[1],size=2,na.rm=T) +
			geom_point(aes(as.numeric(row.names(ptable)),ptable[,2]),color=mcolor[6],size=2,na.rm=T) +
			geom_point(data=data.frame(fitval,dof), aes(dof,fitval),shape=8, color=mcolor[3],size=5,na.rm=T) + 
			ggtitle(paste(fitmetric, "Noise Baseline and Equivalent Measure \nwith Constant Noise Level")) +
			xlab("Degrees of Freedom") +
			ylab(ylb) + 
			geom_text(x=tx, y=0.80, label=paste0("fitval = ",fitval,"   dof = ",dof), color=mcolor[3], hjust=1, size=4) +
			geom_text(x=tx, y=0.75, label=paste0("fitEquiv = ",r2k), color=mcolor[6], hjust=1, size=4) +
			geom_text(x=tx, y=0.70, label=paste0("fitNoise = ",pctlist[1]), color=mcolor[1], hjust=1,size=4) + 
			geom_text(x=tx, y=0.65, label=paste0("Improved fitval Measure"), color=mcolor[4], hjust=1, size=4)
			
			
	if(fitmetric=="R2"){plt <- plt +
			geom_ribbon(aes(x=as.numeric(row.names(ptable)), ymin=ptable[,2], ymax=1),fill=mcolor[4],alpha=0.3,na.rm=T)}
			
						
	if(fitmetric=="RMSE"){plt <- plt +
			geom_ribbon(aes(x=as.numeric(row.names(ptable)), ymax=ptable[,2], ymin=0),fill=mcolor[4],alpha=0.3,na.rm=T)}
			
			

	#if pct_r2 is T, plot the noise level where pct=fitval
	if(plot_pctr2){plt <- plt + 
			geom_point(aes(as.numeric(row.names(ptable)),ptable[,3]),color=mcolor[2],size=2) +
			geom_text(x=tx, y=0.60, label=paste0(fitmetric," Noise Percentile = ",pctlist[2]), color=mcolor[2], hjust=1,size=4,na.rm=T)}
	
	#if fitval is in the noise, show the improved but still noisy R2 values in a black ribbon.
	if(any(ptable[,2]<ptable[,1]) & fitmetric=="R2"){ plt <- plt +
			geom_ribbon(aes(x=as.numeric(row.names(ptable)), ymin=ptable[,2], ymax=ptable[,1]),fill=mcolor[5],alpha=0.7,na.rm=T) +
			geom_text(x=tx, y=0.55, label=paste0("Unacceptable Noise"), color=mcolor[5], hjust=1, size=4) +
			geom_point(data=data.frame(fitval,dof), aes(dof,fitval),size=4,shape=8, color=mcolor[3],na.rm=T) }			
			
	#if fitval is in the noise, show the improved but still noisy RMSE values in a black ribbon.
	if(any(ptable[,2]>ptable[,1]) & fitmetric=="RMSE"){ plt <- plt +
			geom_ribbon(aes(x=as.numeric(row.names(ptable)), ymin=ptable[,1], ymax=ptable[,2]),fill=mcolor[5],alpha=0.7,na.rm=T) +
			geom_text(x=tx, y=0.55, label=paste0("Unacceptable Noise"), color=mcolor[5], hjust=1, size=4) +
			geom_point(data=data.frame(fitval,dof), aes(dof,fitval),size=4,shape=8, color=mcolor[3],na.rm=T) }			

	return(plt)
	#return(ppp)
}


##############################################################################################################################
#
# get a boilerplate of common useful stats
#
#
#
#' Print boilerplate stats
#'
#' Prints a standard set of stats for a given degree of freedom
#'
#' @param dof  an integer
#' @param pctlist
#' @param ndecimals an integer
#' @param dist a random number distribution function
#' @param order a real number
#' @param ... any argument that functions within this routine might use
#'
#' @return data frame
#'
#' @examples
#' fitStats(6, dist='normal', sd=0.3)
#'
#' @export
#' fitStats()
fitTable <- function(dof, pctlist=c(0.90,0.95,0.99), ndecimals=3, dist='normal', order=5, ... ) {

	o=order
	nsamples = 10^order
	R2baselines		<- NoiseTable(doflist=dof,pctlist=pctlist, order=order, fitmetric='R2',  dist=dist, ndecimals=ndecimals,...)
	RMSEceilings		<- NoiseTable(doflist=dof,pctlist=pctlist, order=order, fitmetric='RMSE',dist=dist, ndecimals=ndecimals,...)	
	
	np = length(pctlist)
	r2b= unlist(R2baselines[1,])
	rmsec = unlist(RMSEceilings[1,])
	df <- data.frame(dof=rep(dof,np),percentiles=paste0(pctlist*100,"%"),R2=r2b,RMSE=rmsec)
	rownames(df)=c()
	name.width <- max(sapply(names(df), nchar))
	format(df, width = name.width, justify = "centre")

	return(df)
}
##############################################################################################################################
#
# get threshold percentiles
#
#
#
fit <- function(measured_value, dof, fitmetric, order=6, ndecimals=2, ... ){
	df <- pcdfs(dof,fitmetric=fitmetric,order=order,...)
	if(fitmetric=="R2"){current_percentile <- df$cdf[df$fitval>=measured_value][1]}  #list all cdfs where fitval>=measured and take first one in the list
	if(fitmetric=="RMSE"){
		df$fitval_rev <- rev(df$fitval)
		current_percentile <- df$cdf[df$fitval_rev<measured_value][1]}
		current_percentile <- sprintf("%1.2f%%", 100*current_percentile)
	return(current_percentile)
}
