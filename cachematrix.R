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
    # Note: 'inv' is a free variable
    inv <- NULL

    # Create a set of functions that will be responsible for caching and
    # retrieval of the matrix and of its inverse.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv

    # Note: the matrix can be accessed by calling foo$get(), and its inverse
    # (in its current cache state) can be obtained by calling foo$getinverse().
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
    #
    # First, see if the inverse has already been cached...
    #
    inv <- x$getinverse()
    if(!is.null(inv)) {
        #
        # ...and, if so, simply return the cached data:
        #
        message("getting cached data")
        return(inv)
    }
    
    #
    # If there is no cached value, call 'solve', forwarding all the extra
    # parameters passed to this function.
    #
    data <- x$get()
    inv <- solve(data, ...)

    #
    # Finally, cache the inverse for later re-use.
    #
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
    print(inv %*% M$get())
    message("Calling cacheSolve() again")
    inv <- cacheSolve(M)
    print(inv)
    message("Making a different cache matrix")
    M2 <- makeCacheMatrix(inv)
    message("Let's see how its inverse looks like (should be NULL)")
    print(M2$getinverse())
    message("Now let's see what happens when we invert it")
    inv2 <- cacheSolve(M2)
    message("Print out both inverses")
    print(M$getinverse())
    print(M2$getinverse())
    message("This should be identity again")
    inv %*% inv2
}
