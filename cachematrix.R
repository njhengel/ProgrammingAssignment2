## The first set of code creates the Inverse of a Matrix via the solve function
## The second set of code runs this function but checks cache first
## Solution utilizes example work from assignment as base

## First Set

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
            ## Sets intial value to NULL
      set <- function(y) {
            x <<- y
            ## X is set by looking for a else-where defined y
            m <<- NULL
      }
      get <- function() x
            ## looks for x
      setinv <- function(solve) x <<- solve
            ## creates the intial inverse via solve
      getinv <- function() m
            ## retrives inverse
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Second set: looks for existing data, else creates

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
            ## checks to see it already exists
      data <- x$get()
            ##returns cached value
      m <- solve(data, ...)
      x$setinv(m)
      m
}
