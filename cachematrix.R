## The following two functions cahe the potentially time consuming
## computation of the inverse of a matrix: When the inverse of a matrix 
## has to be computed repeatedly while the contents of a mtrix do not change
## it is less expensive, computationally and time-wise to cache the inverse and retreive it later.


## makeCacheMtrix creates a matrix, which is a list containing a function to

## - set the value of the matrix
## - get the value of the matrix
## - set the inverse of the matrix
## - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes  the inverse of the matrix created with the above function.
## However, it first checks to see if the inverse has already been computed.
## If so, it gets the inverse from the cache and skips the computation. 
##  Otherwise, it computes the inverse of the matrix and sets the result of the 
## inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
