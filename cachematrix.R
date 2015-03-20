## Below are two functions that are used to create a special object 
## that stores a matrix and cache's it inversed

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
## set the value of the matrix in cache
set <- function(y) {
        x <<- y
        m <<- NULL
        ## get the value of the matrix in cache
        get <- function() x
        ## set inversed matrix in cache
        setsolve <- function(solve) m <<- solve
        ## get inversed matrix in cache
        getsolve <- function() m
        ## return list object
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Try retrieve the inverse from the cache
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Computes new inverse of matrix
        data <- x$get()
        m <- solve(data, ...)
        ## Set new inverse matrix in cache
        x$setsolve(m)
        m        
        
}
