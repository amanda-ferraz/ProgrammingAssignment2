## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inve <- NULL
        set <- function(k) {
                x <<- k
                inve <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inve <<- inverse
        getInverse <- function() inve
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inve <- x$getInverse()
        if (!is.null(inve)) {
                message("getting cached data")
                return(inve)
        }
        mat <- x$get()
        inve <- solve(mat, ...)
        x$setInverse(inve)
        inve
}
