## Matrix inversion is usually a costly computation and their may be some benefit to caching
##the inverse of a matrix rather than compute it repeatedly.  In this code I am  writing a 
##pair of functions that cache the inverse of a matrix.

##The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This cacheSolve function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and
##the matrix has not changed), then the cachesolve should retrieve the
##inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}

## Testing out the cacheSolve and makeCacheMatrix function. 

b <-makeCacheMatrix(matrix(c(4,7,2,6), 2,2))
cacheSolve(b)
