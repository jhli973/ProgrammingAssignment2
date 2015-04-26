## This is assignment 2 project which includes two functions which return a
## inverse matrix. The makeCacheMatrix function is to cache the inverse matrix
## if a inverse matrix exists. If no inverse matrix exists, the second function 
## cacheSolve function will reverse the matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  getmat <- function() x
  setinv <- function(inverse) mat <<- inverse
  getinv <- function() mat
  
  list(getmat=getmat, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getinv()
  if(!is.null(mat)){
    message("getting cached matrix")
    return(mat)
  }
  data <- x$getmat()
  mat<- solve(data, ...)
  x$setinv(mat)
  x$getinv()
  mat
}
