makeCacheMatrix <- function(x = matrix()) {
# Example input: Insert matrix e.g x<-matrix(rnorm(64),8,8)
## To check cached values: 
# xMat<-makeCacheMatrix(x)  # Run the function
# parent.env(xMat$getenv())$m  # Check the cached mean
# environment(xMat$getmean)  # refer to environment of "m"
m<-NULL   
evn <- environment()  
y<-NULL 

setmatrix<-function(y){  
	x<<-y  
	m<<-NULL 
}
  
getmatrix<-function() x  
setinverse<-function(solve) m<<- solve  
getinverse<-function() m  
getenv<- function() environment()

list (setmatrix=setmatrix, getmatrix = getmatrix,   
setinverse = setinverse,
getinverse = getinverse,
getenv = getenv)

}
## The function "cacheSolve" returns the inverse of the matrix that is 
# returned by makeCacheMatrix function, e.g. xMat$getmatrix()

cacheSolve <- function(xMat= m(), ...) {
	
	m <- xMat$getinverse() 
	if(!is.null(m)){ 
		if(xMat$setmatrix() == xMat$getmatrix()) { 
    	matrix<-xMat$get()
    	m<-solve(matrix, ...)
    	xMat$setmatrix(m)
    	return(m) 
    	}
    	 
    	y <- xMat$getmatrix() 
    	xMat$setmatrix(y) 
    	m <- solve(y, ...) 
    	xMat$setinverse(m) 
    	m    # return the inverse
	}
}
