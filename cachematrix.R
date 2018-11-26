## This function generates a special list in which each element is a function to set and retrieve
## either a matrix or it's inverse. 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL 
        set <- function(y) {
                x <<- y
                i <<- NULL 
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Retrieves the inverse of a matrix. Use the cached value if it has already been calculated.
## Otherwise calculate the inverse, cache the result and then return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	i <- x$getinv()
	if(!is.null(i) {
		message("getting cached data")
		return(i)
	}

	# if it reaches this point it means that the inverse of the matrix is not in the cache
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
}
