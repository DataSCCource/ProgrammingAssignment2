## Functions for calculating and caching the inverse of a given matrix
## Example: --------------------------------------------------------------------------------------
## source("cachematrix.R")                      ## load this script
## m <- matrix(rnorm(1000*1000), 1000,1000)     ## create random 1000x1000 element test matrix 'm'
## mat <- makeCacheMatrix(m)                    ## create caching matrix 'mat' from 'm'
## inv <- cacheSolve(mat)                       ## calculate inverse for the first time
## inv <- cacheSolve(mat)                       ## get the cached inverse the second time
## -----------------------------------------------------------------------------------------------

## makeCacheMatrix(matrix) acts as a container/class that holds a matrix 'x' 
## and (if calculated) its inverse 'm'. 'm' will be NULL if the inverse wasn't 
## calculated yet or if 'x' was changed (using the set-function).
## It also provides functions for getting and setting 'x' and 'm'.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve(matrix) returnes the inverse of a given matrix.
## The matrix will only be calculated the first time and cached, so there 
## is no reason to calculate it again (as long as the matrix is unchanged).
cacheSolve <- function(x, ...) {
    m <- x$getInverse()  
    if(!is.null(m)) {  ## if inverse is already cached
        message("getting cached inverse")
        return(m)
    }
    
    ## otherwise calculate inverse, cache and return it
    data <- x$get() 
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
