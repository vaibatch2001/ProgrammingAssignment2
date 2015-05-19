## This script allows users to create the inverse of a matrix and cache the latter
## for future retrieval. As long as the matrix remains the same, this process will
## lead to better performance, especially for large matrices 

## This function creates a matrix and contains the following key utility functions:
## setinverse - caches the inverse of the matrix
## getinverse - retrieves the cached inverse (or NULL)

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


## This function computes the inverse of matrix returned by the makeCacheMatrix() function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve() function will retrieve the inverse from the cache.
## If the cache is empty, the function will calculate the inverse and store it in the cache

cacheSolve <- function(x, ...) {
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
