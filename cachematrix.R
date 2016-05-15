## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
## Below are a pair of functions that cache the inverse of a matrix.


## Function makeCacheMatrix create a list containing a function to:
## 1. set the value of the matrix via set()
## 2. get the value of the matrix via get()
## 3. set the value of the inverse matrix via setinverse()
## 4. get the value of the inverse matrix via getinverse()

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##  Function cacheSolve calculates the inverse of the matrix
##  created by function makeCacheMatrix.
##  If the inverse has already been calculated (and the matrix 
##  has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
