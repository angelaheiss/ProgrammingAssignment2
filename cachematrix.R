## These functions create and store a square matrix then
## solve for and cache its inverse.

## Sets and gets the elements of the matrix
## and sets and gets the elements of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function () inv
	list (set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## Checks to see if inverse is already calculated
## Returns cached answer, otherwise solves for inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
