### Assignment: Caching the Inverse of a Matrix

Matrix inversion is usually a costly computation and there may be some
benefit to caching the inverse of a matrix rather than computing it
repeatedly (there are also alternatives to matrix inversion that we will
not discuss here). Your assignment is to write a pair of functions that
cache the inverse of a matrix.

The following functions are for evaluation:

1.  `makeCacheMatrix`: This function creates a special "matrix" object
    that can cache its inverse.
2.  `cacheSolve`: This function computes the inverse of the special
    "matrix" returned by `makeCacheMatrix` above. If the inverse has
    already been calculated (and the matrix has not changed), then
    `cacheSolve` should retrieve the inverse from the cache.

The following function sets and gets the inverse of the matrix.

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



The following function Caches the inverse of the matrix from the above function. 

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

### Test Cases to test the cache functions.

         amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
         amatrix$get()         # Returns original matrix
         cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
         amatrix$getInverseVector()  # Returns matrix inverse
         cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
         amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
         cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
         amatrix$get()         # Returns matrix
         amatrix$getInverseVector()  # Returns matrix inverse


