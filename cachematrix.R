## Some functions that allow caching the inversion of a matrix.
## This can avoid re-doing expensive calculations.

## Accepts a matrix as input (or creates an empty one on default input).
#Creates 4 subfunctions. This is similar to classes with methods in OOP.
# set: sets the value of the matrix
# get: returns the value of the matrix
# setsolve: sets the value of the inversion of the matrix and writes it to the variable i in the 
#  parent function's environment by using the <<- operator
# getsolve: gets the value of the inversion of the matrix
#Sample calls:
# Initialise:
#  a <- makeCacheMatrix()
# Prints a list of functions in makeCacheMatrix():
# $a
# Set: a$set(matrix(seq(1:4),2))
# Get: a$get()
# Set the inverse: a$setsolve(solve(a$get()))
# Get the inverse: a$getsolve()
# Test by verifying that the inverse of the inversion matches the original vector:
# a$get() == solve(a$getsolve())
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# Takes a makeCacheMatrix object
# Returns its inversion, using the inversion cached in the
#  makeCacheMatrix, if it is already calculated.
#  Otherwise calculate and cache it.
# Example calls:
# a <- makeCacheMatrix(matrix(seq(1:4),2))
# cacheSolve(a)
# Returns cached copy on subsequent runs
# cacheSolve(a)
cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached matrix inversion")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
  ## Return a matrix that is the inverse of 'x'
}
