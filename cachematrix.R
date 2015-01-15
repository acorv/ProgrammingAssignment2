## These functions implement a 'caching matrix'. The matrix caches its inverse and returns the cached value if it's requested more than once.
##
## Example:
##
## m <- matrix(c(2, 4, 3, 1, 5, 7, 2, 5, 1), nrow=3, ncol=3) 
## cm <- makeCacheMatrix(m)
## cacheSolve(cm)  ## This call calculates the inverse
## cacheSolve(cm)  ## This call returns the cached value


## Builds a 'cached matrix' from a matrix. 
## A 'cached matrix' can store the result of solving the original matrix
## The cached result can be set/got using setinverse/getinverse 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## Returns de inverse of 'caching matrix' x. If x was already solved, it returns the original 
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    message("calculating inverse")
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
