## cachematrix.R
## Author: Hugo van den Berg
##
## cachematrix.R provides a set of complementary functions to cache results of
## computationally costly matrix operations. Caching is realised by leveraging
## lexical scoping, for which a two-step procedure is required.
##
## First of all the matrix is converted to CacheMatrix using the
## makeCacheMatrix function. This CacheMatrix can then be passed to the
## cacheSolve function instead of the original matrix to solve the matrix.
## Solving the matrix is performed only once as the result is stored in the
## CacheMatrix.

## makeCacheMatrix creates a cached version of the input matrix which can be
## passed to the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) im <<- inverse
    getInverse <- function() im
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve solves a matrix, stored as a CacheMatrix, and stores the result
## in the cache. Subsequent calls using the same CacheMatrix return the cached
## result instantly.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message('Returning cached result')
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInvers(inverse)
    inverse
}
