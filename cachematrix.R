# Creating a “matrix”  that can cache its inverse.

makeCacheMatrix <- function (x = matrix()){
  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



# Computes the inverse of the "matrix" created by the makeCacheMatrix. 

cacheSolve <- function (x = matrix(), ...){
  inv <- x$getinv()
  if (!is.null(inv)){
    message("GETTING CACHED DATA")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  return(inv)
}