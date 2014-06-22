## These two functions combined generate an inverse of a matrix. The inverse is
## then cached and can be called without repeating the calculation. 

##makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
}

## cacheSolve: This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed), then 
##the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
##"x" here is a placeholder for makeCacheMatrix(x) when you actually run the 
##code in the console.
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
