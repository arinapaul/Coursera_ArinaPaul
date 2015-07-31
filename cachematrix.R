## These pair of functions caches the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix is a function that takes in a matrix, sets the inverse
## to null since its a new matrix, then creates getters and setters for both
## the matrix we passed in, and the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)
    m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. cacheSolve is a function that takes in an object and checks to see if
## the object has a function of getInverse, for the matrix inverse. If
## the inverse of the object is NOT null, it retreives the value from
## that environment and returns, stopping the function. Otherwise, it
## uses the get function to obtain the matrix, solves the inverse,
## and sets the inverse of the object to the new inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

