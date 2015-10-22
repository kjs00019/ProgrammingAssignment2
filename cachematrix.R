## makeCacheMatrix creates a matrix that can be stored outside
##  of the local environment. cacheSolve calculates the inverse
##  of the matrix if m is NULL. Otherwise it retrieves the cached
##  inverse.


## makeCacheMatrix creates a matrix and a null matrix,
##  sets the null matrix to the inverse matrix and stores it 
##  in the global environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix if m is NULL.
## If the matrix is not NULL then it retrieves the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
