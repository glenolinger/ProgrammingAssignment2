## The following two functions, makeCacheMatrix() and cacheSolve() can solve and cache
## the inverse matrix. If the inverse has already been calculated then the cacheSolve()
## will retrieve the inverse matrix from the cache, otherwise the inverse matrix will 
## be calculated.

## makeCacheMatrix() creates a special "matrix" object from the input matrix 
## that can cache its inverse. 

makeCacheMatrix <- function(mat = matrix()) {
    iMat <- NULL
    set <- function(y) {
        mat <<- y
        iMat <<- NULL
    }
    get <- function() mat
    set_iMat <- function(invMat) iMat <<- invMat
    get_iMat <- function() iMat
    list(set = set, get = get,
         set_iMat = set_iMat,
         get_iMat = get_iMat)
}

## cacheSolve() computes the inverse of the special "matrix" returned by
## makeCacheMatrix().

cacheSolve <- function(mat, ...) {
    iMat <- mat$get_iMat()
    dMat <- mat$get()
    if(!is.null(iMat)) {
        message("Getting the cached inverse matrix.")
        return(iMat)
    }
    
    message("Calculating the inverse matrix.")
    iMat <- solve(dMat, ...)
    mat$set_iMat(iMat)
    iMat
}
