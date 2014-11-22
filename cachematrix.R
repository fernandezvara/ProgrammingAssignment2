# This are a pair of functions that allow to work with matrixes and its inversed.

## makeCacheMatrix
#
# This function returns a 'cached' matrix.
#
# params
#    x: matrix
# return:
#   no return
# functions:
#   get: returns the matrix we are working with
#   set: sets the matrix to work with
#   getInverse: returns the inverse matrix, makes the calculation the 
#               first time called. It caches the result.
#   setInverse: sets the inverse of the matrix we are working with
# 
# Example:
# 
# m = matrix(1:4, 2, 2)
# mcached = makeCacheMatrix(m)
# mcached.getInverse()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  # set matrix to work with
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  
  # actual matrix, the one setted with set
  get <- function() {
    x
  }
  
  # set inverse matrix
  setInverse <- function(m) {
    inverse <<- m
  }
  
  # get (return) inverse matrix
  getInverse <- function() {
    inverse
  }
  
  # interface 
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
    )
}

## cacheSolve
#
# This function returns the inverse matrix of 'cached' matrix.
#
# params
#    x: matrix ('cached', won't work with a normal one)
# return:
#    inverseMatrix: matrix
# 
# Example:
# 
# m = matrix(1:4, 2, 2)
# mcached = makeCacheMatrix(m)
#
# cacheSolve(m) # Fails, you have passed a normal matrix
# cacheSolve(mcached) # Works! You passed a 'cached' matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # actual inverse
  inverseMatrix <- x$getInverse()
  
  # return inverse if exists
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  # inverse not exists, so calculate it again
  actualMatrix <- x$get()
  inverseMatrix <- solve(actualMatrix, ...)
  x$setInverse(inverseMatrix)
  
  inverseMatrix
}
