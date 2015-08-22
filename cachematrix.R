## makeCacheMatrix
## Optionally takes a Matrix as Input and provides functions to 
##      gets a Matrix
##      sets a Matrix and keeps in  in cache. 
##      gets the inverse of the Matrix 
##      and sets the inverse of Matrix
##      
##      Sample Usage:
##      m<-matrix(rnorm(9,10),3) # creates a 3x3 matrix with random variables within 10
##      x<-makeCacheMatrix(m)
##      cacheSolve(x)   # Should give you an inverse matrix
##      cacheSolve(x)   # Should give a message and get from cache.
##      x$set(m)        # sets a new matrix and nullifies the cache.
##      cacheSolve(x)   ## Should give you an inverse ( not from cache) without message.
##      
##      Way to test
##      round(cacheSolve(x) %*% m)  # should give a unit matrix.
##

makeCacheMatrix <- function(mat = matrix()) {
	invMat <- NULL
	set <- function(newMat) {
		mat <<- newMat
		invMat <<- NULL
	}
	get <- function() mat
	setinv <- function(newInvMat) invMat <<- newInvMat
	getinv <- function() invMat
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##  cacheSolve uses makes makeCacheMatrix to get the inverse of the Matrix. 
## if it already available( exists in cache ) it returns from cache without recalculating
# if its not available , it gets the  matrix and computes the inverse using solve method and sets the inverse in cache.

cacheSolve <- function(x, ...) {
        invMat <- x$getinv()
	if(!is.null(invMat)) {
		message("Getting cached inverse Matrix..")
		return(invMat)
	} 
	mat <- x$get()
	invMat <- solve(mat)
	x$setinv(invMat)
	invMat
}
