## The following functions contain the solution to Programming Assignment 2.
## They can be used to calculate the inverse of an invertible matrix using
## a thin wrapper for the matrix that caches the result of the calculation.

## Creates a wrapper of the given matrix that can cache its inverse.
## Warning: using wrapper$set(y) will change the underlying matrix and
## discard the cached inverse, but modifying the wrapped matrix
## manually can lead to an inconsistent state!
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Takes a matrix wrapper created with makeCacheMatrix(x) and calculates the
## inverse matrix unless it has already been calculated and cached. Any
## additional arguments will be passed to the solve() function.
## Warning: if the inverse matrix has already been calculated and cached,
## solve() won't be invoked again, so the additional arguments will be ignored!
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (is.null(inv)) {
        inv <- solve(x$get(), ...)
        x$setinverse(inv)
    }
    inv
}
