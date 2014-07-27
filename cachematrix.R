## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes in a matrix and calculates the inverse
## it also sets a variable 'setinverse' to that calculated value
## where that value was initially set at 'NULL'

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <-function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i<<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse= setinverse,
             getinverse = getinverse)
}


## cacheSolve checks the value of the setinverse variable created by
## makeCacheMatrix and if it isn't NULL it returns that value rather 
## than re-calculating the inverse of the matrix.

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

