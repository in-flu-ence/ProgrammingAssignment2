## JHU Coursera R Programming Assignment #2
## Author: Harris Ngan
## Date: 15 Dec 2017
##
## This R Script aims to reduce costly computation for matrix inversion by caching. The functions get an input
## matrix and calcuate its inverse. It will subsequently cache this calculation for future retrieval. When a new
## matrix is set, a new inverse will be calculated and cached.


## makeCacheMatrix creates a list of functions used for retrieval and setting of the input matrix and its
## corresponding inverse for caching.

makeCacheMatrix <- function(x = matrix()) {
    ## Initiating inv_mat as NULL
    inv_mat <- NULL
    
    ## Assign new value for input matrix and clear cached inverse matrix
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    
    ## Retrieve input matrix and display
    get <- function() x
    
    ## Store calculated inverse to inv_mat
    set_inv <- function(invert) inv_mat <<- invert
    
    ## Search and Get the current inv_mat
    get_inv <- function() inv_mat
    
    ## Create a list of functions that can be used in the parent environment using the $ operator
    list(set = set, get = get, setinv = set_inv, getinv = get_inv)
}


## cacheSolve creates the workflow for executing the functions created in makeCacheMatrix. It takes in an input matrix.
## and search for previous cached record. It will output either a new calculated inverse or a cached inverse 
## (if available)
## Assumption: The matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        
        ## Attempt to retrieve a cached inverse matrix and return matrix if found
        inv_mat <- x$getinv()
        if(!is.null(inv_mat)) {
            message("getting cached data")
            return(inv_mat)
        }
        
        ## Attempt to calculate a new inverse matrix
        data <- x$get()
        inv_mat <- solve(data)
        x$setinv(inv_mat)
        
        inv_mat
}
