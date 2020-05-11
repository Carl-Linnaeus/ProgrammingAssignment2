## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  invMatrix = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    invMatrix <<- NULL
  }
  get = function() x
  setinvM = function(inverse) invMatrix <<- inverse 
  getinvM = function() invMatrix
  list(set=set, get=get, setinv=setinvM, getinv=getinvM)
}

cacheSolve <- function(x, ...) {
  ## output of makeCacheMatrix()inv()
  
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$get
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}



