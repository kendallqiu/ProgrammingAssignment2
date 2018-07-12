## 1st commit SHA-1 hash identifier: b8465ac59aeb0c5b19d6da3800a43d18a26e1ac7
## Assignment 2: Lexical Scoping--caching the inverse of a matrix

## The first function creates a special "matrix", which is a list containing a function to set/get the value of the matrix; set/get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) a <<- inverse
    getinverse <- function() a
    list(
        get = get,
        set = set,
        setinverse = setinverse,
        getinverse = getinverse)
}

## The second function calculates the inverse of the special "matrix", which created with the makeCacheMatrix function. It first checks to see if the inverse has already been calculated. Then it will get the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    a <- x$getinverse()
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    data <- x$get()
    a <- solve(data, ...)
    x$setinverse(a)
    a
}
