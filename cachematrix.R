## The pair of functions will cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
#change mean to inverse and m to inv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <-function () inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special matrix returned by makeCatchMatrix
## if the inverse has already been calculated then cachesolve should retrieve the inverse from the cache
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv
}
data <- makeCacheMatrix(matrix(1:4, 2, 2))
data$get()
data$getinverse()
cacheSolve(data)

