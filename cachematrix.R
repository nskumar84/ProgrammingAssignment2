## Following functions cache the inverse of a matrix and retrieve the inverse of a matrix from the cache if the inverse has  
## already been calculated.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function()x
        setinverse <- function(invmat) mat <<- invmat
        getinverse <- function() mat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        mat <- x$getinverse()
        if(!is.null(mat)){
                message("getting cached data")
                return(mat)
        }
        matdata <- x$get()
        mat <- solve(matdata, ...)
        x$setinverse(mat)
        mat
}