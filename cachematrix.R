## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(z = matrix()) {
        inve <- NULL
        set <- function(k) {
                z <<- k
                inve <<- NULL
        }
        get <- function() z
        setInverse <- function(inverse) inve <<- inverse
        getInverse <- function() inve
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'z'
        inve <- z$getInverse()
        if (!is.null(inve)) {
                message("getting cached data")
                return(inve)
        }
        mat <- z$get()
        inve <- solve(mat, ...)
        z$setInverse(inve)
        inve
}
