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
    list(M = x, M_inv = FALSE)
}

##
## A few simple tests for 'makeCacheMatrix'
##
makeCacheMatrix.test <- function() {
    M <- makeCacheMatrix(matrix(c(1, -0.25, -0.25, 1), nrow = 2, ncol = 2))
    M
}

##
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.
##
cacheSolve <- function(x, ...) {
    if (!x$M_inv) {
        x$M_inv <- solve(x$M, ...)
    }
    x$M_inv
}

##
## A few simple tests for 'cacheSolve'
##
cacheSolve.test <- function() {
    M <- makeCacheMatrix.test()
    inv <- cacheSolve(M)
    inv
}
