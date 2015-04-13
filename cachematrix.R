## Contains two functions which provides support for caching a inverted matrix. This may speed 
## up a programm if inverse of a matrix is required more than once.


## The first function, makeCacheMatrix creates a special "vector", which is really a list
## containing a function to:
##
## set: set the matrix to invert
## get: get the matrix to invert
## setinverse: sets the value of the inverse
## getinverse: gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## This returns the inverse of a matrix. If the function is called the first time
## it computes the inverse of the matrix and caches the result. If the function
## finds a cached result, it skips the computation and returns the cached matrix.
## The matrix passed to the function must be invertible.

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
