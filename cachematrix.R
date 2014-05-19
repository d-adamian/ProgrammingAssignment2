## makeCacheMatrix function creates 'matrix' object with 4 functions:
## get, set, getInverse, setInverse. This object internally stores the 
## matrix and its inverse. Inverse is initially NULL and is set to NULL
## if underlying matrix is changed by 'set' function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)  
}


## cacheSolve function returns inverse of a given 'matrix' object.
## If the matrix has already been inverted and cached, the function returns
## cached value. Otherwise, matrix inverse is computed and stored in cache

cacheSolve <- function(x, ...) {  
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
