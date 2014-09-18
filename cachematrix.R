## Put comments here that give an overall description of what your
## functions do

# The function calculates the inverse of a matrix

## Write a short comment describing this function

# The function caches the inverse of matrices. It does so by saving y in a different scope
# using the setsolve method. It uses the getsolve method to retrieve it next time if 
# the input matrix x has not changed

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## Write a short comment describing this function

# The function takes in a matrix and see whether it is cached and has not changed
# If yes, the function returns the cached results, without calling the "solve" function
# Otherwise the function calls "solve" function to calculate the inverse and save 
# the results to cache by calling setsolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}

