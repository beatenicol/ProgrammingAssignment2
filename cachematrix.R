## The two functions cache the inverse of a matrix. 

## makeCacheMatrix creates a list containing functions to 
# set the value of the matrix (set)
# get the value of the matrix (get)
# set the value of the inverse of the matrix (setinv)
# get the value of the inverse of the matrix (getinv)

# this list is used as the input to cacheSolve()
            
makeCacheMatrix <- function(x = matrix()) {
 
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() {
    x
  }
  setinv = function(solve) {
    inv <<- solve 
  }
  getinv = function() {
    inv
  }
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# cacheSolve takes the output of makeCacheMatrix()
# and returns the inverse of the original matrix
cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  matrix = x$get()
  inv = solve(matrix, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  #return the inversed matrix
  inv
}



