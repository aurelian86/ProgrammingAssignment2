## The two functions below calculate the inverse of a matrix and 
## cache the result to save up compute time if the inverse of the
## same matrix needs to be calculated multiple times
##
## How to use
##
## source("cachematrix.R")
## mymatrix <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2) ##this builds a 2 x 2 matrix
## cacheSolve(makeCacheMatrix(mymatrix))
##
## The inverse of mymatrix will be printed
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4


## The makeCacheMatrix returns a vector of functions that allows:
## 1. setting and getting the value of a matrix
## 2. setting and getting the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function returns the inverse of a matratrix returned by the makeCacheMatrix() function.
## If the inverse is already cached it returns that value, if not, it will calculate and cache it using the setinv() function above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
