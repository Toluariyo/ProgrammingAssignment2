
## Creates a special “matrix” object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Store a new matrix and clear any cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Retrieve the stored matrix
  get <- function() x
  
  ## Cache the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## Retrieve the cached inverse (or NULL if not yet cached)
  getinverse <- function() inv
  
  ## Return a list of methods for interacting with this object
  list(
    set        = set,
    get        = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Computes (or retrieves) the inverse of the “matrix” created by makeCacheMatrix.
## If the inverse is already cached, it is returned immediately.
cacheSolve <- function(x, ...) {
  ## Try to get the cached inverse
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  ## Otherwise, compute the inverse, cache it, and return it
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}