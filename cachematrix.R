# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly.
# We will write two functions which achieve above.

# makeCacheMatrix: This function creates a special "matrix" object
#                  that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        mInverse <- NULL
        #set matrix if new matrix is passed.
        setMatrix <- function(y) {
                x <<- y
                mInverse <<- NULL
        }
        # get existing matrix
        getMatrix <- function() x
        # set matrix inverse value
        setInverse <- function(inverse) mInverse <<- inverse
        # get existing inverse value
        getInverse <- function() mInverse
        # list of all methods.
        list(set = setMatrix, get = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}

# cacheSolve: This function computes the inverse of the special
#             "matrix" returned by makeCacheMatrix above. 
#              If the inverse has already been calculated 
#               (and the matrix has not changed), then cacheSolve
#               should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        # get matrix inverse value
        mInverse <- x$getInverse()
        # check if inverse exists. If exists return existing value.
        if(!is.null(mInverse)) {
                message("getting cached matrix inverse")
                return(mInverse)
        }
        # get stored data.
        data <- x$get()
        # calculate matrix inverse.
        mInverse <- solve(data, ...)
        # set calculated inverse value.
        x$setInverse(mInverse)
        mInverse
}