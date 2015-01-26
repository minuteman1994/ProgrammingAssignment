### Put comments here that give an overall description of what your
### functions do
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

### Write a short comment describing this function
## When we want the inverse of the matrix, we create a special matrix which can 
## have the inverse cached for faster return. We use the setinverse and getinverse commands
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


### Write a short comment describing this function
## If the inverse of the matrix has already been calculated by a prior run of this function,
## we return the stored inverse, otherwise we calculate the inverse and store for faster recall
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}