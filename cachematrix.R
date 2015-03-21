## These functions are used to compute the inverse of a matrix and cache the result.

## Creates a cache of the inverse of the given matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'. It uses a cache system. When a
## value has been computed, it is saved in the cache. If the same value is
## needed again, it will be retrieved from the cache
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
