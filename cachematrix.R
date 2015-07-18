## The following functions are used to cache time-consuming computations,
## in this case, the inverse of matrix. Cache is taken up when the computation 
## is similar to what has been done previously in the loop.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Set initial inv value to NULL
    inv <- NULL
    
    ## The 4 basic functions for caching
    get <- function() x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    
    ## Register the functions
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function return a matrix that is the inverse of 'x'

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Checking the cache
        m_inverse <- x$getinv()
        if(!is.null(m_inverse)) {
            message("getting cached data")
            return(m_inverse)
        } 
        
        ## If not found, calculate the inverse
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

