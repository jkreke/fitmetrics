Consistency_Check <- function(x,y){

#this routine is only useful when the function is used outside of the fitmetrics package.
#if the function is used within the package, the routine that calls it generates the arguments beforehand so they're already check for consistency, class, length,..

#however, the function might be used outside of fitmetics package and so it needs some checking.
# the function should be written so that there are two matricies that are compared (basically).  #For instance, R2 has xs and ys or dependent and independent variables.  RMSE also has two #arguments (same, independent and dependent variables).  So the commonality in these 


	gclasses <- c("numeric", "integer")
	
	if(class(x) %in% gclasses & class(y) %in% gclasses & length(x)==length(y)){
		x <- matrix(x,nrow=1)
		y <- matrix(y,nrow=1)
		} else if(class(x)=="matrix" & class(y)=="matrix"){
			
			
		} else {
			stop("x & y classes or lengths different")
		}
	if(ncol(x)==1 | ncol(y)==1){stop("can not compute fit with only one value")}
	



	if(class(x)     == "matrix" & 
	   class(y)     == "matrix"  &
	   dim(x)[[1]]  == dim(y)[[1]]  & 
	   dim(x)[[2]]  == dim(y)[[2]]
	   ){		
		N=dim(x)[[1]]
		dof=dim(x)[[2]]
		}
		
		mlist <- list(x,y,N,dof)
		return(mlist)
		
}