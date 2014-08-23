## Functions for calculating and caching the inverse of a given matrix
## -------------------------------------------------------------------------------------
## Example:
## source("cachematrix.R")					## load this script
## m <- matrix(rnorm(100*100), 100,100) 	## create random 100x100 element test matrix
## mat <- makeCacheMatrix(m)				## create caching matrix 'mat' from 'm'
## inv <- cacheSolve(mat)					## calculate inverse for the first time
## inv <- cacheSolve(mat)					## get the cached inverse
## -------------------------------------------------------------------------------------

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
