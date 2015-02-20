
## Date - 2/16/2015
## Code - Caching Matrix Inverse for Efficiency

## Two functions that enable caching of the matrix inverse
## Function 1 - makeCacheMatrix() solves the inverse of a matrix using the solve()
##              function and assigns it to a global variable I
## Function 2 - cacheSolve() attempts to find the inverse of an input matrix term
##              and if the inverse is pre calculated for that input matrix
##              via the first function, and if not calculates it afresh

## Function 1
## makeCacheMatrix() computes the inverse of a matrix and caches it.
## The input for this function comes from the call of the cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
    
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) I <<- solve
    getinverse <- function() I
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## Function 2
## cacheSolve() is the function that actually takes a matrix input and is widely used
## It in turn calls the makeCacheMatrix and checks if an inverse already exists
## If it does, then the cache is used, else the inverse is calculated

cacheSolve <- function(x) {
    
    if(!is.null(I)) {
        message("Getting cached data for Inverse")
        return(I)
    }
    I <- getinverse[['x']]
    data <- x$get()
    I <- solve(x)
    x$setinverse(I)
    I
    
}
