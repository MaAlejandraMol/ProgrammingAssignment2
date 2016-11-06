## Make a pair of functions that
## catch the inverse of a matrix

## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
                }
        get <- function() x
                setsolve <- function(solve) s <<- solve
                        getsolve <- function() s
                                list(set = set, get = get,
                                     setsolve = setsolve,
                                     getsolve = getsolve)
}


## Computes the inverse of the special matrix returned by the CacheMatrix above, if not calculated before

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
                }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)  ## Return a matrix that is the inverse of 'x'
        s
}
