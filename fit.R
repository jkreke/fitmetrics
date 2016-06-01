##############################################################################################################################
#
# The R-squared calculation based on two numeric vectors of equal length
#
#
#' R-squared
#'
#' Calculates R-squared given vectors for x and y
#'
#' @param x a vector or matrix of real numbers
#' @param y a vector or matrix of real numbers
#'
#' @return a vector of numbers with length equal to the number of trials (N)
#'
#' @examples
#' R2( seq(2:10),(3*rnorm(2:10)+4) )
#'
#' @export
#'R2
R2 <- function(x, y){
	
	gclasses <- c("numeric", "integer")
	if(class(x) %in% gclasses & class(y) %in% gclasses & length(x)==length(y)){
		x <- matrix(x,nrow=1)
		y <- matrix(y,nrow=1)
		} else if(class(x)=="matrix" & class(y)=="matrix"){
			
			
		} else {
			stop("x & y classes or lengths different")
		}
	if(ncol(x)==1 | ncol(y)==1){stop("can not compute R2 with only on value")}

	if(class(x)     == "matrix" & 
	   class(y)     == "matrix"  &
	   dim(x)[[1]]  == dim(y)[[1]]  & 
	   dim(x)[[2]]  == dim(y)[[2]]
	   ){		
		N=dim(x)[[1]]
		dof=dim(x)[[2]]
		}
	
	
	#define R2 components x and y				
	xb		<- rowSums(x)/dof
	xd		<- x-xb

	yb 		<- rowSums(y)/dof					#get means for each row
	yd 		<- y-yb								#get delta

	#calculate R2 numerator
	n1		<- xd*yd
	n1s		<- rowSums(n1)
	num		<- n1s*n1s							#numerator

	#calculate R2 denominator
	d1		<- xd^2
	d2		<- yd^2
	d1s		<- rowSums(d1)
	d2s		<- rowSums(d2)
	den		<- d1s*d2s							#denominator

	#calculate R2 (this is an array of R2 calculations based on noise)
	R2vector	<- num/den						#R2
	
	
	
	return(R2vector)
	
	
}



##############################################################################################################################
#
# The rmse calculation based on two numeric vectors of equal length
#
#
#' rmse
#'
#' Calculates rmse given vectors for y and y_pred
#'
#' @param obs a vector or matrix of real numbers
#' @param mdl a vector or matrix of real numbers
#'
#' @return a vector of numbers with length equal to the number of trials (N)
#'
#' @examples
#' rmse(seq(3:20), (seq(3:20)+rnorm(18)^2))
#'
#' @export
#'rmse
rmse <- function(obs, mdl){
	#need to know if these are vectors of numbers or matrices
	#need to know that they're the same dimensions
	#need to know which dimension is dof and which is N
	#if vectors, convert to matricies

	gclasses=c("numeric", "integer")
	if(class(obs) %in% gclasses & class(mdl) %in% gclasses & length(obs)==length(mdl)){
		obs <- matrix(obs,nrow=1)
		mdl <- matrix(mdl,nrow=1)
		} else if(class(obs)=="matrix" & class(mdl)=="matrix"){
			
			
		} else {
			stop("obs and mdl classes or lengths different")
		}


	if(class(obs)     == "matrix" & 
	   class(mdl)     == "matrix"  &
	   dim(obs)[[1]]  == dim(mdl)[[1]]  & 
	   dim(obs)[[2]]  == dim(mdl)[[2]]
	   ){		
		N=dim(obs)[[1]]
		dof=dim(obs)[[2]]
		}

	#get delta
	ed 		<- obs-mdl								#predicted ep									

	#calculate RMSE numerator
	n1		<- ed*ed							#square the individual elements (not matrix multiplication)
	n1s		<- rowSums(n1)						#sum the individual rows
	num		<- sqrt(n1s)						#sq root of individual elements -> numerator

	#calculate RMSE denominator
	den		<- sqrt(dof)						#denominator

	#calculate rmse (this is an array of rmse calculations based on noise)
	rmsevector	<- num/den

	return(rmsevector)

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
#' @obs a vector of random numbers dof*N long
#' @mdl a vector of random numbers dof*N long
#'
#' @return a data frame
#'
#' @export
#' gen_R2df()
gen_R2df <- function(dof, N, bw, obs, mdl){


#establish matricies
x		<- matrix(mdl, nrow=N, ncol=dof)
y  		<- matrix(obs, nrow=N, ncol=dof)	#make a matrix of N rows by n columns

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
#' @obs a vector of random numbers dof*N long
#' @mdl a vector of random numbers dof*N long
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
#' @param user a function created by the user that accepts two matricies of numbers N x dof (rows x cols)
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


##############################################################################################################################
#
# find the general trend (Positive or Negative) of the user function
#
#
#' Determine the general trend (general, overall slope) of the fitmetric function
#'
#' Calculates the fitmetric function at two distant points and calculates the slope using lineal regression.
#'
#' @param fun as function R2, rmse, or user (user-defined function)
#'
#' @return a character string ("Positive", "Negative", "Flat", "Uncertain")
#'
#' @examples
#' utrend(R2)
#' utrend(user)
#'
#' @export
#' utrend()
utrend <- function(fun){

	#initiate varaiables
	nsamples=3*10^5
	xA = 3
	xB = 1000
	fmt <- "%1.2f"

	if( fun=="R2"   ){func=R2
		} else if(
		fun=="rmse" ){func=rmse
		} else if(
		fun=="user" ){func=user
		} else {stop("uncertain fitmetric function")}
		
	#generate two matricies at two distant points to determine general function trend
	#use rnorm with default paramters
	#pointA
	matA <- matrix(rnorm(nsamples), ncol=xA)
	matB <- matrix(rnorm(nsamples), ncol=xA)
	vA <- func(matA, matB)
	vA <- sum(vA)/length(vA)
	vA <- sprintf(fmt,vA)
	
	#pointB
	matA <- matrix(rnorm(nsamples), ncol=xB)
	matB <- matrix(rnorm(nsamples), ncol=xB)
	vB <- func(matA, matB)
	vB <- sum(vB)/length(vB)
	vB <- sprintf(fmt, vB)

	df <- data.frame(v=c(vA,vB), x=c(xA,xB))
	mdl <- lm(v~x,data=df)
	slope <- mdl$coefficient[[2]]
	
		   if(slope> 0)	{trend <- "Positive"
	} else if(slope==0)	{trend <- "Flat"
	} else if(slope< 0)	{trend <- "Negative"
	} else 				{trend <- "Uncertain"
		}


return(trend)

}


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
if(fitmetric=="rmse")	{fitdf <- gen_RMSEdf( dof, N, bw, rnums1, rnums2)}
if(fitmetric=="user")	{fitdf <- gen_userdf( dof, N, bw, rnums1, rnums2)}

return(fitdf)
}


##############################################################################################################################
#
#	determine the baseline noise level (fitNoise) for a corresponding number of degrees of freedom(dof) and noise percentile(pct)
#
#
#' Find The Threshold Noise Level
#'
#' Finds the threshold noise level for a certain fit metric
#'
#' @param dof an integer
#' @param pct a real number between 0 and 1
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric ("R2", "rmse", or "user")
#' @param dist a random number distribution function
#' @param trend a character string "Positive", "Negative", "Flat", "Uncertain" which describes the general slope of the fitmetric function.
#' @param ... any argument that functions within this routine might use
#'
#' @return a real number
#'
#' @examples
#' fitNoise(7)
#' fitNoise(4, fitmetric='rmse')
#'
#' @export
#' fitNoise()
fitNoise <- function(dof, pct=0.95, ndecimals=2, fitmetric='R2', dist='normal', trend=NULL,...){

		if(is.null(trend)){fitmetric_trend = utrend(fitmetric)} else {fitmetric_trend=trend}
		cdf <- pcdfs(dof, ndecimals=ndecimals, fitmetric=fitmetric, dist=dist, ...)[,c("fitval","cdf")]
		if(fitmetric_trend=="Positive"){
			c_pct <- as.numeric(1-as.numeric(pct))
			c_val <- cdf$fitval[cdf$cdf<=c_pct]
			c_val <- rev(c_val)
			nb <- c_val[1]

		} else if(fitmetric_trend=="Negative") {
			nb <- cdf$fitval[cdf$cdf>=pct][1]		#nb is the value of cdf just at the point where it's just >= p
		} else {
			stop("uncertain fitmetric trend")
		}
		nb <- nb + rnorm(1)*10^(-(ndecimals+2))  #add a small random number to remove any binning errors.
		fmt <- paste0("%1.",ndecimals,"f")
		nb <- as.numeric(sprintf(fmt,nb))
	

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
#' @param measured_value a real number
#' @param dof an integer
#' @param pct a real number between 0 and 1
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric ("R2", "RMSE", or "user")
#' @param trend a character string "Positive", "Negative", "Flat", "Uncertain" which describes the general slope of the fitmetric function.
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
fitEquiv <- function(measured_value, dof, pct=0.95, ndecimals=2, fitmetric="R2", trend=NULL, ...){
	fitval=measured_value
	noiselevel <- fitNoise(dof=dof, pct=pct, ndecimals=ndecimals, fitmetric=fitmetric, trend=trend, ...)

	#determine if noise trends upward (positive) or downward (negative)
	if(is.null(trend)){fitmetric_trend = utrend(fitmetric)} else {fitmetric_trend=trend}
	if(fitmetric_trend=="Negative"){
			eqfitval <- (fitval-noiselevel)/(1-noiselevel + 0.00000000001)
		} else if(fitmetric_trend=="Positive"){
			eqfitval <- fitval/noiselevel
		} else 
			stop("uncertain fitmetric trend")
	
	
	#make eqfitval consistent with the number of decimal places in noiselevel.  may have to tweak this for positive trend functions
	fl <- floor(noiselevel)								
	nd <- rep(0,length(fitval))
	if(noiselevel-fl>0){
		nd <- nchar(sapply(strsplit(as.character(noiselevel), ".",fixed=T), "[[", 2))} 
	fmt <- paste0("%1.",ndecimals,"f")
	eqfitval <- as.numeric(sprintf(fmt,eqfitval))

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
#' @param doflist
#' @param pctlist
#' @param order a real number
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric ("R2", "rmse", or "user")
#' @param trend a character string "Positive", "Negative", "Flat", "Uncertain" which describes the general slope of the fitmetric function.
#' @param ... any argument that functions within this routine might use
#'
#' @return a data frame
#'
#' @examples
#' NoiseTable(fitmetric="rmse")
#'
#' @export
#' NoiseTable()
#
NoiseTable <- function(doflist=NULL, pctlist=NULL, order=4, ndecimals=2, fitmetric="R2", trend=NULL, ...){
	if(is.null(doflist)){doflist=c(4,8,16,32,64,128)}
	if(is.null(pctlist)){pctlist=c(0.7,0.9,0.95,0.99)}
	if(is.null(trend)){fitmetric_trend=utrend(fitmetric)} else {fitmetric_trend=trend}
	nds <- length(doflist)	#need test here for pos integer
	nps <- length(pctlist)	#need test here for nums >0 and <1
	rownams = as.character(doflist)
	colnams = as.character(pctlist)

	
	shell <- matrix(nrow=nds, ncol=nps)
	noisetab <- matrix(mapply(function(x,i,j) fitNoise(doflist[i], pctlist[j], order=order, ndecimals=ndecimals, fitmetric=fitmetric, trend=fitmetric_trend, ...), shell,row(shell),col(shell)), nrow=nds, ncol=nps)

	noisetab <- as.data.frame(noisetab)
	colnames(noisetab) <- colnams
	rownames(noisetab) <- rownams
	return(noisetab)
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
#' @param fitmetric a character string naming a standard fit metric ("R2", "rmse", or "user")
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

dfx <- pcdfs(dof=dof, order=order, dist=dist, fitmetric=fitmetric, ...)
N = 10^order
dist2 <- sapply(dist, cap1)
mxy = max(dfx$pdf)
maxx <- max(dfx$fitval)
	if(fitmetric=='R2'){fmet   <- expression(R^2);gtitle="R-squared"}
	if(fitmetric=='rmse'){fmet <- expression(RMSE);gtitle="RMSE"}
	if(fitmetric=="user"){fmet <- expression(user);gtitle="user"}
plot <- ggplot(dfx) + 
		geom_point(aes(fitval, pdf),size=1) +
		ylim(0,mxy) +
		xlab(fmet) + 
		ylab("Probability Density") +
		ggtitle(paste(gtitle, "Probability Density Function")) +
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
#' @param fitmetric a character string naming a standard fit metric ("R2", "rmse", or "user")
#' @param ... any argument that functions within this routine might use
#'
#' @return ggplot object
#'
#' @examples
#' plotcdf(5, dist='uniform', fitmetric='rmse')
#'
#' @export
#' plotcdf()
plotcdf <- function(dof, order=4, dist='normal', fitmetric='R2', ...){  		#need to explicitly state distribiution here in order to get it into the plot title

r2cdf <- pcdfs(dof=dof, order=order, dist=dist, fitmetric=fitmetric, ...)
cdf <- NULL													#see http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when.  Need this to eliminate a note during R CMD check
N = 10^order
dist2 <- sapply(dist, cap1)
	if(fitmetric=='R2'){fmet   <- expression(R^2);gtitle="R-squared"}
	if(fitmetric=='rmse'){fmet <- expression(RMSE);gtitle="RMSE"}
	if(fitmetric=='user'){fmet <- expression(user);gtitle="user"}
mxy <- max(r2cdf$cdf)
maxx <- max(r2cdf$fitval)
plot <- ggplot(r2cdf) + 
		geom_point(aes(fitval, cdf),size=1) +
		ylim(0,mxy) + 
		xlab(fmet) + 
		ylab("Cumulative Probability") +
		ggtitle(paste(gtitle,"Cumulative Probability Density Function")) +
		geom_text(aes(x=0.95*maxx,y=0.3*mxy,label=paste("Noise Distribution:",dist2,
													"\nDegrees of Freedom:",dof,
													"\nNumber of  Samples:",floor(N))),size=3,hjust=1)

return(plot)
}


##############################################################################################################################
#
# Plot noise levels for a list of percentiles (no more than 5 in the list)
#
#
#' Plot Noise threshold
#'
#' Plots the Noise threshold for each degree of freedom
#'
#' @param doflist a vector of degrees of freedom
#' @param pctlist a vector of percentiles (expressed as fractions)
#' @param order a real number greater than 0 and less than 7
#' @param ndecimals an integer
#' @param fitmetric a character string naming a standard fit metric ("R2", "rmse", or "user")
#' @param ... any argument that functions within this routine might use
#'
#' @return ggplot object
#'
#' @examples
#' plotNoise()
#'
#' @export
#' plotNoise()
plotNoise <- function(doflist=c(2:30), pctlist=c(0.95), order=4, ndecimals=2, fitmetric='R2', ...){
	if(length(pctlist)>5){stop(paste("Too many percentiles to calculate", length(pctlist)))}

	doflist <- doflist[doflist>1] 
	doflim <- min(120, length(doflist))
	doflist <- doflist[1:doflim]
	
	pctlim <- min(5,length(pctlist))
	pctlist <- pctlist[1:pctlim]
	pctlist <- formatC(as.numeric(pctlist),width=(ndecimals+1),format='f',digits=ndecimals,flag='0')
	doflength <- length(doflist)
	pctlength <- length(pctlist)
	
	mcolor <- c("black", "blue", "red", "green", "darkgreen")
	sizes <- c(3.6, 3.2, 2.8, 2.4, 2.0)/2  #make the points of the first plots larger so they can be seen
	#sizes <- c(1, 0.8, 0.6, 0.4, 0.2)   #use these for geom_path instead of geom_point
	
	fitmetric_trend = utrend(fitmetric)
	noisedf <- NoiseTable(doflist=doflist, pctlist=pctlist, order=order, fitmetric=fitmetric, trend=fitmetric_trend, ...)
	mxy <- 0.9*max(noisedf[,1])
	N = 10^order
	plt <- ggplot(noisedf)

	if(length(pctlist)==1){maintitle <- paste("Baseline Noise Level\nfor One Noise Percentile(p)")
						} else {
						maintitle <- paste("Baseline Noise Level\nfor Various Noise Percentiles(p)")
						}
	if(fitmetric=='R2'){fmet   <- paste("R-squared", maintitle);ylb=expression(R^2)}
	if(fitmetric=='rmse'){fmet <- paste("RMSE", maintitle);     ylb=expression(RMSE)}
	if(fitmetric=="user"){fmet <- paste("user", maintitle);     ylb=expression(user)}
	gtitle <- fmet


	if(pctlength>=1){plt <- plt + 
		geom_point(aes(as.numeric(row.names(noisedf)), noisedf[,1]), color=mcolor[1], size=sizes[1])  +
		geom_text(aes(x=max(doflist), y=mxy-0.00, label=paste0("p = ",pctlist[1])), color=mcolor[1], hjust=1, size=4)}
	if(pctlength>=2){plt <- plt + 
		geom_point(aes(as.numeric(row.names(noisedf)), noisedf[,2]), color=mcolor[2], size=sizes[2])  +
		geom_text(aes(x=max(doflist), y=mxy-0.05, label=paste0("p = ",pctlist[2])), color=mcolor[2], hjust=1, size=4)}
	if(pctlength>=3){plt <- plt + 
		geom_point(aes(as.numeric(row.names(noisedf)), noisedf[,3]), color=mcolor[3], size=sizes[3])  +
		geom_text(aes(x=max(doflist), y=mxy-0.10, label=paste0("p = ",pctlist[3])), color=mcolor[3], hjust=1, size=4)}
	if(pctlength>=4){plt <- plt + 
		geom_point(aes(as.numeric(row.names(noisedf)), noisedf[,4]), color=mcolor[4], size=sizes[4])  +
		geom_text(aes(x=max(doflist), y=mxy-0.15, label=paste0("p = ",pctlist[4])), color=mcolor[4], hjust=1, size=4)} 
	if(pctlength>=5){plt <- plt + 
		geom_point(aes(as.numeric(row.names(noisedf)), noisedf[,5]), color=mcolor[5], size=sizes[5])  +
		geom_text(aes(x=max(doflist), y=mxy-0.20, label=paste0("p = ",pctlist[5])), color=mcolor[5], hjust=1, size=4)}
	
	plt <- plt + 
	ggtitle(gtitle) +
	xlab("Degrees of Freedom") +
	ylab(ylb) +
	geom_text(aes(x=max(doflist), y=mxy+0.05, label=paste0("Number of Samples = ",N)), color='black', hjust=1, size=4)

	
	return(plt)
}




##############################################################################################################################
#
# Plot Equiv for a single fitmetric value across a range of dofs
#
#' Plot Fit Equivalent
#'
#' Plots the Fit Equivalent
#'
#' @param measured_value
#' @param doflist
#' @param pct
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
#' @param pct
#' @param order a real number
#' @param plot_pctr2 
#' @param fitmetric a character string naming a standard fit metric ("R2", "rmse", or "user")
#' @param ... any argument that functions within this routine might use
#'
#' @return ggplot object
#'
#' @examples
#' plotConstNoise(0.8, 5)
#' plotConstNoise(0.1, 5, fitmetric="rmse")
#'
#' @export
#' plotConstNoise()
plotConstNoise <- function(measured_value, dof, pct=0.95, order=4, plot_pctr2=F, fitmetric='R2', ...){

	fitmetric_trend <- utrend(fitmetric)
	fitval			<- measured_value
	mcolor 			<- c("red", "blue", "forestgreen", "slategray4", "gray20", "black")
	

	# get the pcdf for this dof
	dfx <- pcdfs(dof=dof, order=order, fitmetric=fitmetric, ...)
	
	doflist = c(2:30)
	pctlist = c(pct)
	
	
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
	
	if(fitmetric=="R2"){	ylb=expression(R^2);  gtitle="R-squared"}
	if(fitmetric=="rmse"){	ylb=expression(RMSE); gtitle="RMSE"}
	if(fitmetric=="user"){  ylb=expression(user); gtitle="user"}
	
	f = fitEquiv(fitval,dof,pct,fitmetric=fitmetric, trend=fitmetric_trend, ...)
	               if(fitmetric_trend=="Negative"){
				          ptable$fitEquiv <- f*(1-ptable[,1]) + ptable[,1]
			} else if(fitmetric_trend=="Positive") {
				          ptable$fitEquiv <- f*(ptable[,1])
			} else {
				          stop("fitmetric trend uncertain")
			}		
	ptable$fitEquiv[(dof-1)]=measured_value   #fitEquiv at dof must equal the measured_value at dof.  ptable will be a little off,due to randomness so make them equal.


	tx = max(doflist[doflength])
	ttx = 2/3*tx
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
			
			
			
	if(fitmetric_trend=="Negative"){plt <- plt +
			geom_ribbon(aes(x=as.numeric(row.names(ptable)), ymin=ptable[,2], ymax=1),fill=mcolor[4],alpha=0.3,na.rm=T)}		#ymax here might be dependent on the metric				
	if(fitmetric_trend=="Positive"){plt <- plt +
			geom_ribbon(aes(x=as.numeric(row.names(ptable)), ymax=ptable[,2], ymin=0),fill=mcolor[4],alpha=0.3,na.rm=T)}
			

	#if pct_r2 is T, plot the noise level where pct=fitval
	if(plot_pctr2){plt <- plt + 
			geom_point(aes(as.numeric(row.names(ptable)),ptable[,3]),color=mcolor[2],size=2) +
			geom_text(x=ttx, y=tval[7], label=paste0(fitmetric," Noise Percentile = ",pctlist[2]), color=mcolor[2], hjust=0,size=4,na.rm=T)}
	
	#if fitval is in the noise, show the improved but still noisy R2 values in a black ribbon.
	if(fitval<ptable[dof,1] & fitmetric_trend=="Negative"){ plt <- plt +
			geom_ribbon(aes(x=as.numeric(row.names(ptable)), ymin=ptable[,2], ymax=ptable[,1]),fill=mcolor[5],alpha=0.7,na.rm=T) +
			geom_text(x=ttx, y=tval[6], label=paste0("Unacceptable Noise (dark area)"), color=mcolor[5], hjust=0, size=4) +
			geom_point(data=data.frame(fitval,dof), aes(dof,fitval),size=4,shape=8, color=mcolor[3],na.rm=T) }			
			
	#if fitval is in the noise, show the improved but still noisy RMSE values in a black ribbon.
	if(fitval>ptable[dof,1] & fitmetric_trend=="Positive"){ plt <- plt +
			geom_ribbon(aes(x=as.numeric(row.names(ptable)), ymin=ptable[,1], ymax=ptable[,2]),fill=mcolor[5],alpha=0.7,na.rm=T) +
			geom_text(x=ttx, y=tval[6], label=paste0("Unacceptable Noise (dark area)"), color=mcolor[5], hjust=0, size=4) +
			geom_point(data=data.frame(fitval,dof), aes(dof,fitval),size=4,shape=8, color=mcolor[3],na.rm=T) }				


	return(plt)
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
fitTable <- function(dof, pctlist=c(0.90,0.95,0.99), ndecimals=2, dist='normal', order=5, ... ) {

	R2baselines		<- NoiseTable(doflist=dof,pctlist=pctlist, order=order, fitmetric='R2',  dist=dist, ndecimals=ndecimals,...)
	RMSEbaselines	<- NoiseTable(doflist=dof,pctlist=pctlist, order=order, fitmetric='rmse',dist=dist, ndecimals=ndecimals,...)	
	userbaselines	<- NoiseTable(doflist=dof,pctlist=pctlist, order=order, fitmetric='user',dist=dist, ndecimals=ndecimals,...)
	
	np	  <- length(pctlist)
	r2b   <- unlist(R2baselines[1,])
	rmsec <- unlist(RMSEbaselines[1,])
	userd <- unlist(userbaselines[1,])
	dfx   <- data.frame(dof=rep(dof,np),percentiles=paste0(pctlist*100,"%"),R2=r2b,RMSE=rmsec, user=userd)
	rownames(dfx) <- c()
	name.width <- max(sapply(names(dfx), nchar))
	format(dfx, width = name.width, justify = "centre")
	
	print(paste("Noise Dist:         ",dist))
	print(paste("Number of Samples:  ", 10^order))
	print(paste("Degrees of Freedom: ",dof))

	return(dfx)
}



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
#' fit(6, dof=10, pct=0.95)
#'
#' @export
#' fit()
#
fit <- function(measured_value, dof, pct, fitmetric="R2", order=6, ndecimals=2, dist='normal', table=TRUE, ... ){
	dfx <- pcdfs(dof,fitmetric=fitmetric,order=order,dist=dist,...)
	
	fitmetric_trend <- utrend(fitmetric)
	if(fitmetric_trend=="Negative"){
		current_percentile <- dfx$cdf[dfx$fitval>=measured_value][1]  #list all cdfs where fitval>=measured and take first one in the list
		}
	if(fitmetric_trend=="Positive"){
		dfx$fitval_rev <- rev(dfx$fitval)
		current_percentile <- dfx$cdf[dfx$fitval_rev<measured_value][1]
		}	
	nlevel <- fitNoise(dof=dof, pct=pct, ndecimals=ndecimals, fitmetric=fitmetric, dist=dist,trend=fitmetric_trend,...)
	fmt <- paste0("%1.",ndecimals,"f")
	

	if(table){
		tfitmetric 	<- "Fit Metric:"
		vfitmetric 	<- fitmetric
	
		tdof		<- "Degrees of Freedom:"
		vdof		<- dof
	
		tdist 		<- "Noise Distribution:"
		vdist 		<- dist
	
		tnsamples 	<- "Number of Samples:"
		vnsamples 	<- 10^order
		
		tmv			<- "Measured Value:"
		vmv			<- measured_value
	
		tnlevel 	<- if(fitmetric_trend=="Positive"){tnlevel <- paste(fitmetric,"Noise Ceiling:")} else {tnlevel <- paste(fitmetric,"Noise Baseline:")}
		vnlevel 	<- nlevel
	
		tdpct 		<- "Min Acceptable Noise Percentile:"
		vdpct 		<- sprintf(fmt, pct)
	
		tapct 		<- "Calculated Noise Percentile:"
		vapct 		<- sprintf(fmt, current_percentile)

		tfiteq 		<- "Fit Equivalent Value:"
		feq    		<- fitEquiv(measured_value=measured_value, dof=dof, pct=pct, ndecimals=ndecimals, fitmetric=fitmetric, dist=dist,...)
		vfiteq 		<- sprintf(fmt, feq)
	
	
		outdf 		<- data.frame(Parameter	=c(tfitmetric,tdof,tdist,tnsamples,tmv,tnlevel,tdpct,tapct,tfiteq), 
									Value	=c(vfitmetric,vdof,vdist,vnsamples,vmv,vnlevel,vdpct,vapct,vfiteq)
							)
	
		}
	
	if(table){return(outdf)} else {return(cat(sprintf(fmt,current_percentile)))}
}

