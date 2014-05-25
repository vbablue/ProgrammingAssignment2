#This function caches the matrix that is passed. Calling this function should pass matrix as an argument. 
#Returns a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseVector <- function(solve) m <<- solve
  getInverseVector <- function() m
  list(set = set, get = get,
       setInverseVector = setInverseVector,
       getInverseVector = getInverseVector)   
}

#This function caches the value of the inverse matrix when called multiple times to facilitate easy and fast retrieval from the memory. 
#Returns a matrix
cacheSolve <- function(x, ...) {
  m <- x$getInverseVector()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseVector(m)
  m
  
}



#Please Ignore for Evaluation/Feedback
#Test Cases for cacheSolve
amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()         # Returns original matrix
cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
amatrix$getInverseVector()  # Returns matrix inverse
cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
amatrix$get()         # Returns matrix
amatrix$getInverseVector()  # Returns matrix inverse
