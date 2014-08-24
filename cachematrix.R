## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix"
##      returned by makeCacheMatrix above.
##      If the inverse has already been calculated
##      (and the matrix has not changed),
##      then cachesolve should retrieve the inverse from the cache.


## Take a common matrix as argument and
## create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {        
        ## When a special "matrix" object is first created from a common matrix,
        ## its inverse has not yet been calculated,
        ## so the cache is set to be empty.
        i <- NULL
        ## When a special "matrix" object already exists,
        ## but the matrix is reset by the set function,
        ## the cache containing any old inverse is also emptied.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## The get function extracts the common matrix
        ## from the special "matrix" object.
        get <- function() x
        ## The setinverse function stores a value in the cache
        ## (meant for the inverse matrix).
        setinverse <- function(inverse) i <<- inverse
        ## The getinverse function returns the cached inverse.
        getinverse <- function() i
        ## Make the four functions above available
        ## together with the special "matrix" object.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute (or retrieve) the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## If the inverse of special "matrix" 'x' has been cached,
        ## retrieve and return it.
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## Otherwise, extract the common matrix from the special "matrix" 'x'
        ## and compute its inverse.
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}