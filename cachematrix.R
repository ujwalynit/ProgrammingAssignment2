## makeCacheMatrix consists of four functions: set, get, setInt, getInv
## Assumption is matrix is invertible
## set function changes the value of x and m in the main function,sets matrix y to x and makes inverse m null
## setInv changes the value of m in the main function

makeCacheMatrix <- function(x = matrix()) {

      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setInv <- function(inv) m <<- inv
      getInv <- function() m
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## cacheSolve verifies the value of inverse m. If it exists it returns m
## if m is null it computes the inverse of matrix, stores it in the object and returns m

cacheSolve <- function(x, ...) {

      m <- x$getInv()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      matrix <- x$get()
      m <- solve(matrix, ...)
      x$setInv(m)
      m
}
