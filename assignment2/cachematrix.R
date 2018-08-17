##################
#The functions recover the inverse matrix value 
#from cache (or calculate) and print it.  
#################

# The MakeCacheMatrix function creates a special "matrix" that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#The "cacheSolve" function get matrix data in the cache if
#the matrix is there. Otherwise this function calculates it.

cacheSolve <- function(x, ...) {

  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}