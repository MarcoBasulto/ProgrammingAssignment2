
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversa <- function(inversa) m <<- inversa
  getinversa <- function() m
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
  
}


## Return a matrix that is the inverse of 'x'. 
# However, it first checks to see if the inverse of 'x' has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. Otherwise, 
#it calculates the inverse of the matrix and sets the inverse in the cache 
#via the setmean function.

cacheSolve <- function(x, ...) {

  m <- x$getinversa()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversa(m)
  m
  
  }
