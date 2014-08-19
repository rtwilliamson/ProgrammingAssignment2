## These functions create a special "matrix" object that is in reality a list
## the cacheSolve function calculated the matrix inverse and caches the solution

## Creates a list object with the utility to store data and set a cached inverse

makeCacheMatrix <- function(data = matrix()) {
	cache_inverse <- NULL
	set <- function(y) {
		data <<- y
		cache_inverse <<- NULL
	}
	get <- function() data
	setinverse <- function(z) cache_inverse <<- z
	getinverse <- function() cache_inverse
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}

## This function allows you to calculate the inverse or return the stored inverse on subsequent calls

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getinverse()
		if(!is.null(inverse)) {
			message("getting cached data")
			return(inverse)
		}
		data <- x$get()
		inverse <- solve(data, ...)
		x$setinverse(inverse)
		inverse
}

