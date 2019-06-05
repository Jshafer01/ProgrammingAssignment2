## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function, "makeCacheMatrix" takes an invertable matrix as the imput and creates a special "matrix", which is 
# really a list containing a function to set the value of the matrix, get the value of the matrix, 
# set the inverse matrix and get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {

  imatrix <- NULL
  setmatrix <- function(y) {
    x <<- y
    imatrix <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(solve) imatrix <<- solve
  getinverse <- function() imatrix
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# The "cacheSolve" function takes the output from the "makeCacheMatrix" function and checks to see if the inverse matrix has 
# already been calculated. If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, it calculates
# the inverse matrix and sets the the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  imatrix <- x$getinverse()
  if(!is.null(imatrix)) {
    message("getting cached data")
    return(imatrix)
  }
  data <- x$getmatrix()
  imatrix <- solve(data, ...)
  x$setinverse(imatrix)
  imatrix
}
