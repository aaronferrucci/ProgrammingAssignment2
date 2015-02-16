## Put comments here that give an overall description of what your
## functions do


# Supply a list of functions to manage a memoized matrix inversion.
makeCacheMatrix <- function(x = matrix()) {
        # Initialize the cached inverse to NULL (not cached)
        inverse <- NULL
        # setters/getters
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse 
        # Return a list of functions.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

# memoized matrix inversion. 
# Bug: The cached inverse matrix is returned even if the '...' arguments
# (passed to solve()) are different than those used to compute the current
# cached inverse. To fix this, consider adding a "solve" to makeCacheMatrix,
# which will save the '...' parameters for comparison upon future invocation
# of cacheSolve().
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

