##This function creates a special "matrix" object that can cache its inverse 'm'
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function (solve) m<<- solve
        getsolve <- function() m
        z<<- list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- z$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        else {data <- z$get()
        m <- solve(data)
        z$setsolve(m)
        m
        }
}
