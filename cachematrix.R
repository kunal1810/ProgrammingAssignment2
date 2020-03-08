## Put comments here that give an overall description of what your
## functions do

## Function to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse  ## set the cache m 
  getinverse <- function() m  ## returns inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function to computes the inverse of the "matrix" from previous function
## if the inverse has already been calculated, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  #check if  m/cache is null or not 
  ## if null, then following if condition initiate's 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
