## The following two functions can be used to cache the inverse of a matrix "x".

## The first function sets the value of the matrix "x", gets the value of "x",
## sets the value of the inverse of "x" and gets the value of the inverse of "x".

## The second function either retrieves the inverse of the matrix "x" if it has
## already been computed, or computes the inverse for the first time.


## Create a  "matrix" object that can cache the inverse of the matrix
makeCacheMatrix <- function(x = numeric()) {
    n <- NULL
    set <- function(y) {
        x <<- y
        n <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) n <<- solve
    getinverse <- function() n
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute the inverse of the "matrix" returned by makeCacheMatrix above OR
## Retrieve the inverse from the cache if the inverse is already calculated
cacheSolve <- function(x, ...) {
    n <- x$getinverse()
    if(!is.null(n)) {
        message("getting cached data")
        return(n)
    }
    data <- x$get()
    n <- solve(data, ...)
    x$setinverse(n)
    n
}