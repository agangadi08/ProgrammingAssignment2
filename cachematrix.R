

##  makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  get <- function()
    x
  set <-  function(y) {
    x <<- y
    inv <<- NULL
    
  }
  
  getinv <-  function()
    inv
  setinv <- function(inverse)
    inv <<- inverse
  
  list(
    get = get,
    set = set,
    getinv = getinv,
    setinv = setinv
  )
  

}


## cacheSolve computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if (!is.null(inv)) {
    message("output matrix is cached inverse")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  
  #cache inverse
  x$setinv(inv)
  # return inv of a matrix
  return(inv)
}
