## The functions in this file are functions for having a matrix and its inverse together, and calculating the inverse.
## makeCacheMatrx cretes a object containing the matrix as well as potentially its inverse and methods getter and setter for them
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solution) inv <<- solution
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve operates on an object created by makeCacheMatrix and returns the precalculated inverse if it exists
## or calculates the inverse, stores it in the object and return the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)){
        message("getting chached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}
