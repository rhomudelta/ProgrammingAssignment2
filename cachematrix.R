## Create cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
            x
    }
    setinverse <- function(inverse) {
        m <<- inverse
    }
    getinverse <- function() {
        m
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solve inverse of matrix and store in/retrieve from cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
