## The function makeCacheMatrix implements an object that stores a matrix
## and its inverse. The function cacheSolve implements methods to compute
## and cache the inverse of the matrix stored using makeCacheMatrix.
## These functions together can be used to cache inverses of frquently used
## matrices.

## makeCacheMatrix implements an object that stores a matrix and its inverse.
## The function also implements set, get, setSolve and getSolve methods used to
## set and access the stored matrix and to get and set the inverse.

makeCacheMatrix <- function(x = matrix()) {
        slv <- NULL
        set <- function(y) {
                x <<- y
                slv <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) slv <<- solve
        getSolve <- function() slv
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

##
## The cacheSolve function when first called on an object made by makeCacheMatrix
## computes the inverse of the matrix using "solve" function, stores/caches the 
## inverse using setSolve() function and returns the inverse.
## Subsequent calls to cacheSolve just return the cached value using the getSolve 
## function.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		slv <- x$getSolve()
        if(!is.null(slv)) {
                message("getting cached data")
                return(slv)
        }
        data <- x$get()
        slv <- solve(data, ...)
        x$setSolve(slv)
        slv
}
