## These functions create a matrix and calculate its inverse.
## When the inverse is calculated, it is also cached.
## If the inverse has already been calculated, the cached version is used.

## Creates a matrix and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)     
}

## Returns the inverse of a matrix
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {  ## Check if inverse has already been calculated
                message("getting cached matrix inverse")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinv(inv)
        inv
}