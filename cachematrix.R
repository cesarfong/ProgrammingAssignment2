## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# Function Needed to create the cached Matrix
# Use this as a way to create a inversable matrix
# myMatrix <- makeCacheMatrix(c(1, -1/4), c(-1/4, 1))
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x   #allows to retrieve the matrix
  setsolve <- function(solve) m <<- solve     # allows to set the inverse value
  getsolve <- function() m          #allows to get the inversed value (via calculation or cached when using cacheSolve function)
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)       #enable the operations
}


## Write a short comment describing this function
## Operation to calculate the inverse of an inversable matrix
## myMatrix <- makeCacheMatrix(c(1, -1/4), c(-1/4, 1))
## cacheSolve(myMatrix)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()    ##retrieve the inverse of the matrix in case is already calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()       ##retrieve the matrix to calculate the inverse.
  m <- solve(data, ...)   ##calculate the inverse matrix
  x$setsolve(m)         ## assign the value
  m
}
