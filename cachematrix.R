# Programming Assignment 2: A pair of functions to cache the inverse of a matrix

# function to create a special "matrix" object that can cache its inverse
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

# function to compute the inverse of the special "matrix" returned by makeCacheMatrix function
# if inverse has already been calculated and matrix has not changed, retrieve the inverse from cache
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Retrieving cache data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  # return m
  m
}


