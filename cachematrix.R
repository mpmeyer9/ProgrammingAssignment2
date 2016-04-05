## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing functions to store, set, and retrieve the inverse of a specific matrix.
## functions are there to be called by cacheSolve which uses the functions to either retrieve or set the inverse matrix

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


## cacheSolve calls the functions contained in the list created by makeCacheMatrix to retrieve a cached inverse matrix or, if
## the inverse matrix has not been created yet, cacheSolve creates the inverse matrix and then uses a function from cacheSolve
## to cache the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
