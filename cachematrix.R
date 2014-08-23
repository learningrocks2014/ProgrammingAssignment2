## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  invmatrix <- function(solve) m <<- solve
  getmatrix<-function() m
  list(set = set, get = get,       
       invmatrix = invmatrix,
       getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has 
# already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$invmatrix(m)
  m
}
