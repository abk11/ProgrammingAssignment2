## Because calculating the inverse of a matrix tends to be very costly,
## in case that we need to compute the inverse of a matrix we have already encountered,
## if we have already recorded its inverse,
## there is no need to recompute it since we can just look it up in the cache.

## I assume that the given matrix is always invertible.

## The first function, makeCacheMatrix, returns a list containing 4 functions.
## Each of them will respectively:
## 1) set the matrix to be inverted
## 2) get the matrix to be inverted
## 3) set the inverse of the original matrix
## 4) get the inverse of the original matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrixToBeInverted <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrixToBeInverted <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(setMatrixToBeInverted = setMatrixToBeInverted, getMatrixToBeInverted = getMatrixToBeInverted,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The second function, cacheSolve, finds the inverse of the matrix originated with the previous function.
## It asks: has the inverse for the given matrix already been computed?
## If TRUE, it looks up in the cache and returns the matrix.
## If FALSE, it computes the inverse, caches the result, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invertedMatrix <- x$getInverse()
  if(!is.null(invertedMatrix)) {
    message("getting cached data")
    return(invertedMatrix)
  }
  matrixToBeInverted <- x$getMatrixToBeInverted()
  invertedMatrix <- solve(matrixToBeInverted, ...)
  x$setInverse(invertedMatrix)
  invertedMatrix
}

## Example:
## > A <- cbind(c(1, 0), c(1, 1))
## > functionsForMatrixA <- makeCacheMatrix(A)
## > functionsForMatrixA$getMatrixToBeInverted()
##       [,1] [,2]
## [1,]    1    1
## [2,]    0    1

## > cacheSolve(functionsForMatrixA)
##      [,1] [,2]
## [1,]    1   -1
## [2,]    0    1
## > cacheSolve(functionsForMatrixA)
## getting cached data
##       [,1] [,2]
## [1,]    1   -1
## [2,]    0    1