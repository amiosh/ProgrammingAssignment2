# returns a matrix inverse (if possible - cached)
# assumes the matrix is invertible

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  matrixInv <<- NULL
  if (!is.na(matrixInv)) matrixInv <<- solve(x)
  getReg <- function() x
  getInv() <- function() matrixInv
  setInv() <- function(i) matrixInv <<- i
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

## return matrix inverse from makeCacheMatrix
## cached if already calculated

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
    }
  inv <- solve(x$getReg)
  x$setInv(inv)
  # Returns a matrix that is the inverse of 'x'
  inv
}
