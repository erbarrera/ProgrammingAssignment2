## Put comments here that give an overall description of what your
## functions do
## A solution to Programming Assignment 2: Caching the inverse of a
## matrix

## Write a short comment describing this function
## Creates a special "vector", which is really a list containing a 
## set of functions to get/set the value of a matrix and to
## get/set the value of its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## Calculates the inverse of the matrix contained in the special "vector" created 
## with the above function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
