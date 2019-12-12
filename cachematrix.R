## Matrix inverse is a high expensive operation; especially
## for large matrices. Cashing previously computed can
## save use some neccessary resources.

## This function initializes a matrix class with four methods:
## 1: set >> to set a new matrix and clear cashede inverse
## 2: get >> to retrieve a previous matrix
## 3: setinverse >> set the inverse in a global scope
## 4: getinverse >> get the inverse of current matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## check if the current matrix inverse has been calculated
## if not it will solve for it, update the setinverse,
## and return the cached inverse.

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
## Test code
X = makeCacheMatrix()
A <- matrix( c(5, 1, 0,3,-1, 2,4, 0,-1),
             nrow=3, byrow=TRUE)
X$set(A)
X$get()
cacheSolve(X)
X$getinverse()

