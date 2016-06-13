## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # Let's check if we have correct input
  if (!is.matrix(x)) {
    stop("Please provide a matrix")
  }
  
  inverted_x <- NULL
  
  set <- function(y) {
    x <<- y
    inverted_x <<- NULL
  }
  
  # Functions for getting and setting cached inv. matrix value
  get <- function() x
  # Inversing the matrix using build in solve() function in R
  set.inverse <- function(solve) inverted_x <<- solve
  get.inverse <- function() inverted_x
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverted_x <- x$get.inverse()
  
  # Do we have cached matrix available?
  if(!is.null(inverted_x)) {
    message("Getting cached inverse matrix")
    return(inverted_x)
  }
  # Let's create inverted matrix in case
  # there's no cached matrix available.
  matrix_to_invert <- x$get()
  inverted_x <- solve(matrix_to_invert)
  x$set.inverse(inverted_x)
  inverted_x
  
}
