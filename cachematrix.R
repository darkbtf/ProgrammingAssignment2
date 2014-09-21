## makeCacheMatrix: As a constructor, recieves a matrix to initialize, returns a list that contains four functions,
##		    which are getter and setter for the original matrix, and for its inverse matrix.

## cacheSolve:      A matrix inverse solver for our data type defined in function "makeCacheMatrix",
##                  receives a cache matrix as parameter, if the inverse has been already calculated,
##                  returns the calculated value, otherwise calculate it by the built-in function "solve()"


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
