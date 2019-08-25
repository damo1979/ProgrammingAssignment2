## These functions are designed to get the inverse of the matrix and hold
## the result in the cache.  If the result is required again the cache will be
## accessed first or it will be recalculated if necessary   

## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  InvMatrix <- NULL
  set <- function(y) {
    x <<- y
    InvMatrix <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(Inverse) InvMatrix <<- Inverse
  getInvMatrix <- function() InvMatrix
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)  
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  InvMatrix <- x$getInvMatrix()
  if(!is.null(InvMatrix)) {
    message("getting cached data") 
    return(InvMatrix)
  }
  data <- x$get()
  InvMatrix <- solve(data, ...)
  x$setInvMatrix(InvMatrix)
  InvMatrix
}

