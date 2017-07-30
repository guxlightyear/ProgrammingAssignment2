## This file contains the functions required to pass Programming Assignment 2 (week 3) 
## in the Coursera course: R Programming


## This function returns a vector with functions to store a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function returns the cached inverse of the given matrix. If it has not been
# cached yet, calculate it, store it in the cache, and return it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
