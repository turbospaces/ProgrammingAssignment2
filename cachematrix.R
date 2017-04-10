
## "makeCacheMatrix" creates a matrix object with 4 associated functions:
## "set" which sets the matrix, "get" which gets the matrix, "setinv" which sets
## inverse of the matrix and "getinv" which gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # @x: a square invertible matrix
  # return: a list containing functions to
  #  1. set the matrix
  #  2. get the matrix
  #  3. set the inverse
  #  4. get the inverse
  # this list is used as the input to cacheSolve()

  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to 'inv'/'x' variable outside of function scope. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse # update 'inv' variable 
  getinv = function() inv # get value of 'inv' variable
  list(set=set, get=get, setinv=setinv, getinv=getinv) # return list of set/get functions
}

## "cacheSolve" function will invert the matrix created by "makeChacheMatrix" if
## it doesn't exist or use existing inverse if it was stored before    
cacheSolve <- function(x, ...) {
  # @x: output of makeCacheMatrix()
  # return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # check if the inverse has already been calculated already
  if (!is.null(inv)){
    # just get invserse from the cache, no need to compute again
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}

#
# Unit test (second cacheSolve gets result from cache and ptints 'getting data from cache')
# x = matrix(rnorm(9),3,3)
# m = makeCacheMatrix(x)
# cacheSolve(m)
# cacheSolve(m)
#
