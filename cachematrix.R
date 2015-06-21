## makeCacheMatrix and cashSolve allow the caching of the result of
## the potentially lengthy calculation of the invertion of a matrix.
## When a matrix is first inverted, it is stored for later use. If
## it is called to be inverted again, the cached version is
## returned. To accomplishthis, a dummy matrix (as a list) is first
## created by makeCacheMatrix. Then the matrix is either inverted or
## the cached version is returned by cacheSolve

# makeCacheMatrix creates dummy matrix as a list of four functions
# used by cacheSolve to check for, cache and retrieve matrix
# inversions. makeCacheMatrix accepts a matrix as it's input and
# assumes that the matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(newInverse) inverse <<- newInverse
        getInverse <- function() inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


#cachSolve takes a 'list-matrix' created by makeCacheMatrix, checks
#to see if there is an inverse cached, returns it if there is,
#inverts it using 'solve()' if the matrix inverse hasn't been cached,
#caches the inverse if newly created and returns the new inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
