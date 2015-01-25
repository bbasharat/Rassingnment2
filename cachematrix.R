##pair of functions that cache the inverse of a matrix
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of matrix
##get the value of the inverse o matrix



makeCacheMatrix <- function(x=matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<-inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if ( ! is.null(m)) {
        print("getting cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setInverse(m)
    m
}

a <- makeCacheMatrix(matrix(1:4,2))
a$get()
a$getInverse()
a$set(matrix(5:8,2))
a$get()
cacheSolve(a)
cacheSolve(a)
a$getInverse()
b = a$getInverse()
a$get() %*% b
