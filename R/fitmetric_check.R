##############################################################################################################################
#
#	determine that the arguments of the fitmetric function have the correct class then convert them to matrices.
#
#
#' Convert fitmetric arguments to matrices.
#'
#' Checks classes of fitmetric arguments and converts them to matrices if they are not already.
#'
#' @param x a vector of class numeric or integer or a numeric matrix
#' @param y a vector of class numeric or integer or a numeric matrix
#'
#' @return a list containing x, y, N(number of rows), and dof(number of columns)
#'
#' @examples
#' vlist <- fitmetric_check(runif(100000),runif(100000))
#' mlist <- fitmetric_check(matrix(rnorm(10000),ncol=10),matrix(10000),ncol=10))
#'
#' @export
#' fitmetric_check()
fitmetric_check <- function(x,y){

#this routine is only useful when the function is used outside of the fitmetrics package.
# when the function is used within the package, the routine that calls it generates the arguments 
# beforehand so they're already checked for consistency, class, length,....

# However, the user may wish to use the function outside of fitmetics package on functions he or she might create and so the arguments need some checking.
# the function should be written so that there are two matrices of Nxdof dimensions.  
# For instance, R2 has x and y or dependent and independent variables.  RMSE also has two 
# arguments (same, independent and dependent variables).  One may consider these variables "model" and "observation" and for this it really does not matter which is which.


#check classes and dimensions.  If they're vectors, make them matrices of 1xdof dimensions.
	gclasses <- c("numeric", "integer")
	
	if(class(x) %in% gclasses & class(y) %in% gclasses & length(x)==length(y)){
		x <- matrix(x,nrow=1)
		y <- matrix(y,nrow=1)
		
		} else if(class(x)=="matrix" & class(y)=="matrix"){
		#dont do anything	
			
		} else {
			stop("x & y classes or lengths different")
			
		}
	if(ncol(x)==1 | ncol(y)==1){stop("can not compute fit with only one value")}
	


#If they're matracies, get their dimensions.  Should be N rows by dof columns.  
#  N is the number of samples.  dof is the number of degrees of freedom.
	if(class(x)     == "matrix" & 
	   class(y)     == "matrix"  &
	   dim(x)[[1]]  == dim(y)[[1]]  & 
	   dim(x)[[2]]  == dim(y)[[2]]
	   ){		
		N=dim(x)[[1]]						#x and y should be the same (checked above)
		dof=dim(x)[[2]]						#doesnt matter which is chosen
		}
		
		
# Return a list of the original inputs (x,y) now 
#  made into matrices if they were not already, along with N and dof.
	mlist <- list(x,y,N,dof)
	return(mlist)
		
}