## These functions can be used to calculed the inverse of a matrix.
## The inverse matrix is calculated only once to save time on subsequent
## calls.

## The makeCacheMatrix takes a matrix and returns a set of functions 
## to get and set the value of the the matrix and get and set the inverse 
## of the matrix. It caches the inversed matrix when calculated.

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


## cacheSolve calculates the inverse of a matrix or if the invere has 
## already been calculated it return the inverse from cache.

cacheSolve <- function(x, ...) {

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
