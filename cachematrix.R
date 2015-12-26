## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function creates the setter and getter methods for matrix
## its inverse. solve function used for calculating the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <- NULL
    }
    get <- function() x
    setinv <- function(solve){
        inv <<- solve
    }
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
## cacheSolve function first call the getter to see if inverse of matrix already
## exists, if yes, retrieves it otherwise generates the inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}