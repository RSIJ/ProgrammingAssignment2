## The two functions below create from an input matrix a "special" matrix, for
## which the inverse will be cached. Caching data is an alternative for re-calculation.

## This function creates a list-object from a matrix as input variable. 
## The list-object is able to cache the inverse of the input-matrix.
## It contains 4 functions:
## 1. set the value of the matrix.
## 2. get the value of the matrix.
## 3. set/calculate the inverse of the matrix.
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInvMat <- function(solve) im <<- solve
    getInvMat <- function() im
    list(set = set, 
         get = get,
         setInvMat = setInvMat,
         getInvMat = getInvMat
         )
}

## This function returns the inverse of the special matrix, created 
## with the function makeCacheMatrix.
## The inverse will be calculated or, if the calculation already have been done before,
## retrieve from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getInvMat()
    if (!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    message("calculating inverse matrix")
    data <- x$get()
    im <- solve(data, ...)
    x$setInvMat(im)
    im
}
