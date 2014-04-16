
## [makeCacheMatrix] is a function that creates a special object, that is
##  a list of sub-functions (something like Java "methods"):
## 	- [setMat] set the matrix in the principal function environment;
##	- [getMat] get the matrix;
##	- [setInvMat] set the inverse of the matrix in the principal function environment;
##	- [getInvMat] get the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	 
	setMat <- function(y){
			x <<- y
			inv <<- NULL
	}
	
	getMat <- function() { x }
	
	setInvMat <- function(invmat) { inv <<- invmat }
	
	getInvMat <- function() { inv }
	
	list(setMat = setMat, getMat = getMat, setInvMat = setInvMat, getInvMat = getInvMat)
}


## [cacheSolve] is a function that controls if
## the inverse matrix has already been calculated
## and stored (returns it with a message), otherwise 
## it makes the computation at the moment and sets it in the cache. 
## As you can note, this function call three "methods"
## created in [makeCacheMatrix].

cacheSolve <- function(x, ...) {
	inv <- x$getInvMat()
	
	if(!is.null(inv)){
			message("Inverse Matrix is in the cache!")
			return(inv)
	}

	aux <- x$getMat()

	inv <- solve(aux)

	x$setInvMat(inv)

	inv
}
