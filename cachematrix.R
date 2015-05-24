makeCacheMatrix <- function()
{
    # This function creates a special "matrix" object 
    # that can cache its inverse.

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(newinverse) inv <<- newinverse
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
    
}


cacheSolve <- function(x, ...)
{
    # computes the inverse of the special "matrix" returned by makeCacheMatrix 
    # above. If the inverse has already been calculated
    # (and the matrix has not changed), 
    # then the cachesolve should retrieve the inverse from the cache.
    
    # get cached inverse; if found return the value and exit function
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # if cached inverse was null compute the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # update cached value
    x$setinv(inv)
    
    # return
    inv
}
