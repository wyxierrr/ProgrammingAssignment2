## Function for caching the inverse of a matrix
## make a function called "makeCacheMatrix" that creates a special Matrix object that cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## W## This function will check to see if the inverse of the matrix object created above
## has already been calculated. If it has, it will pull from the cache. If not,
## it will calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        ## Return a "matrix" that is the inverse of x
}
