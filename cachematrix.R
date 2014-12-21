## makeCacheMatrix and cacheSolve, together, cache the inverse of a matrix to
## be retrieved, rather than recalculated, when necessary

###############################################################################
## makeCacheMatrix creates a special "matrix" that can cache its inverse, which
## is really a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix's inverse
## 4. get the value of the matrix's inverse
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the inverse inv as null
    inv <- NULL
    
    ## Define the set() function to set the matrix value to the passed matrix
    ## and the matrix's inverse as null
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Define the get() function to return the matrix
    get <- function() x
    
    ## Define the setinverse() function to set the inverse of the matrix to the
    ## given parameter
    setinverse <- function(inverse) inv <<- inverse
    
    ## Define the getinverse() function to return the inverse of the matrix,
    ## which will be null if setinverse() has not been run
    getinverse <- function() inv
    
    ## Return the special "matrix" object
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

###############################################################################
## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated, then
## cacheSolve retrieves the inverse from the cache and skips the computation.
## Otherwise, it solves the matrix and caches the inverse via the setinverse()
## function.
###############################################################################
cacheSolve <- function(x, ...) {
    ## Get the cached inverse of the matrix, if it exists
    inv <- x$getinverse()
    
    ## If the inverse exists in the cache (i.e., inv is not null), return it
    ## with a message noting that it has already been cached
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If the inverse did not exist in the cache, store the matrix locally, ...
    data <- x$get()
    
    ## ... calculate its inverse, ...
    inv <- solve(data, ...)
    
    ## ... cache the inverse in the special "matrix", ...
    x$setinverse(inv)
    
    ## and return the inverse
    inv
}

## Note that the pair of functions is not closed. That is, setinverse() can be
## called on the special "matrix" from anywhere, and can change the cached
## inverse to any storable object. It would be nice to only allow setinverse()
## to be called from cacheSolve().
