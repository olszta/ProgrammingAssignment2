##
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it.
##
## The two functions below -- 'makeCacheMatrix' and 'cacheSolve' -- implement
## a simple memoization scheme for matrix inversion.
##

##
## This function creates a special "matrix" object that can cache its inverse. 
##
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

##
## A few simple tests for 'makeCacheMatrix'
##
makeCacheMatrix.test <- function() {
    message("Creating a fresh invertible matrix") 
    M <- makeCacheMatrix(matrix(c(1, -0.25, -0.25, 1), nrow = 2, ncol = 2))
    print(M)
    M
}

##
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.
##
cacheSolve <- function(x, ...) {
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

##
## A few simple tests for 'cacheSolve'
##
cacheSolve.test <- function() {
    M <- makeCacheMatrix.test()
    message("Inverse before cacheSolve()")
    print(M$getinverse())
    inv <- cacheSolve(M)
    message("Inverse after cacheSolve()")
    print(M$getinverse())
    message("This should be identity")
    inv %*% M$get()
}
