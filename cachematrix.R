## These functions create an object for storing a matrix, caching
## its inverse, and checking the cache to avoid wasteful computation.

## makeCacheMatrix creates an object for storing/modifying and 
## accessing a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve uses makeCacheMatrix to check if it has a cached inverse.
## If there is no cached inverse, it calculates and caches the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
