## The two functions below, makeCacheMatrix, and cacheSolve, reduce the high 
## computational costs associated with computing the inverse of a matrix by 
## retreiving previously calculated matrix inverses from a cache. 


## makeCacheMatrix creates a "matrix" object, which is really a list of
## functions, that, when called can calculate and cache it's inverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve first checks to see if the inverse for the given matrix has
## already been calculated and cached. If it has been, the result is printed. if
## the inverse is yet to be computed, cacheSolve computes the inverse, caches it
## and prints the result.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m           ## Return a matrix that is the inverse of 'x'
        
}
