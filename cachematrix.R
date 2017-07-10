## This R script is used to cache the matrix inverse computations. 
## It is implemented using the following two functions
## makeCacheMatrix is used to create a matrix object (a list),
## that is used to store the cached value of the inverted matrix,
## and functions to retrieve the matrix value as well as the 
## cached inverse value

## This function creates a special "matrix" object that can cache
## its inverse. It is implemented as a list that contains the 
## elements for storage and retrieval of the matrix as well 
## as the inverse

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y){
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setInv <- function(solve) invM <<-solve
  getInv <- function () invM
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM <- x$getInv()
  if(!is.null(invM)) {
    message("getting cached inverse")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data, ...)
  x$setInv(invM)
  invM
}
