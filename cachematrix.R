## This pair of functions work to compite the inverse of a square matrix. Once
## computed these values will be stored in the cache to prevent recalculation.


## Create a new list of required values for cacheMatrix. getinv is initialised
## as NULL, to indicate the inverse has not yet been calculated.
makeCacheMatrix <- function(x = matrix()) {
            inv_mat <- NULL
            set <- function(y) {
                  x <<- y
                  inv_mat <<- NULL
            }
            get <- function() x
            setinv <- function(invert) inv_mat <<- invert
            getinv <- function() inv_mat
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## Loads in a makeCacheMatrix list and checks if inv_mat is cached.
## If already present it returns this value, if not it will be calculated.
cacheSolve <- function(x, ...) {
      inv_mat <- x$getinv()
      if(!is.null(inv_mat)) {
            message("getting cached data")
            return(inv_mat)
      }
      data <- x$get()
      inv_mat <- solve(data, ...)
      x$setinv(inv_mat)
      inv_mat
}
