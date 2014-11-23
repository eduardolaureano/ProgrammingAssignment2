## This .R will contain the full solution with both needed functions

## The initial function "makeCacheMatrix" will create a matrix that inverts 
## the original matrix. 
## It uses the same logic of the make vector example
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## The second  function "cacheSolve" will check the cache for a pre-computed result 
## If a cached result doesn't exist, it will calculate it and update the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getsolve()
    if(!is.null(i)) {
        message("returning cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
}
