## Lexical Scoping Assignment
## A pair of functions that cache the inverse of a matrix

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The following function, cacheSolve() calculates the inverse of the special "vector" created with the first function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the mean from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
## the cache via the setInverse function.


cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if( !is.null(i) ) {
                message("...getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i 
}