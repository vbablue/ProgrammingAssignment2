#This function caches the matrix that is passed. Calling this function should pass matrix as an argument. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseVector <- function(solve) m <<- solve
  getInverseVector <- function() m
  matrix(set = set, get = get,
       setInverseVector = setInverseVector,
       getinversevector = getinversevector)
  
  
}

cacheSolve <- function(x, ...) {
  m <- x$getInverseVector()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseVector(m)
  m
  
}

