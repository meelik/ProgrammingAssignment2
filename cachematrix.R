## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Those functions cache the inverse of a matrix.

## makeCacheMatrix() creates a special "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    if ((nrow(x) <= 0) || (ncol(x) <= 0)) {
        message("Warning! At least one dimension of your matrix is missing!")
    } else if (nrow(x) != ncol(x)) {
        message("Warning! Your matrix is not a squared matrix!")
    }
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inversed) inv <<- inversed
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    if (nrow(data) != ncol(data)) {
        message("Error! Your matrix is not a squared matrix!")
        return
    }

    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
