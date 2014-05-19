# Coursera R Programming (rprog-003) Assignment 2
#
# This is the implementation of the functions for programming
# assignment 2
#
# https://www.github.com/seawaffle/ProgrammingAssignment2/cachematrix.R
#
# To test:
# > x <- matrix(c(0,2,4,6), nrow=2, ncol=2)
# > y <- makeCacheMatrix(x)
# > cacheSolve(y)
# [,1] [,2]
# [1,] -0.75  0.5
# [2,]  0.25  0.0
# > cacheSolve(y)
# getting cached data
# [,1] [,2]
# [1,] -0.75  0.5
# [2,]  0.25  0.0
# > x %*% cacheSolve(y)
# getting cached data
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1

# makeCacheMatrix creates a list with the following functions:
# 1:  set the matrix (set)
# 2:  get the matrix (get)
# 3:  set the inverse of the matrix (setInverse)
# 4:  retrieve the inverse of the matrix (getInverse)
#
# It is intended to be used along with the cacheSolve function,
# which computes and sets the inverse matrix.  Input must be an
# invertable matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# cacheSolve takes the output of makeCacheMatrix.  If the inverse 
# has not been set, it will compute it using the solve function, 
# otherwise it will return the cached value.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
