## This script contains two function: makeCacheMatrix for storing matrix and its inverse and cacheSolve for calculating the matrix inverse and sotring it into makeCacheMatrix


## makeCacheMatrix is an object/list of functions for storing matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrix_inverse <<- inverse
  getinverse <- function() matrix_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function operates over makeCacheMatrix structure and stores matrix inverse into it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minverse <- x$getinverse()
  if(!is.null(minverse)) {
    message("getting cached data")
    return(minverse)
  }
  matrix <- x$get()
  minverse <- solve(matrix)
  x$setinverse(minverse)
  minverse
}

### Testing the implementation
x <- matrix(c(1,2,3,4),2,2) # Creating 2x2 invertable matrix
cache_matrix <- makeCacheMatrix(x) # Make a cache structure out of the matrix
cache_matrix$get() # Get the matrix to test that it is set
cacheSolve(cache_matrix) # cache the inverse of the matrix
cache_matrix$getinverse() # check that the inverse is stored


