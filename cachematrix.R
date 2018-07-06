## The 'makeCacheMatrix' takes a matrix arguement and stores the
## the environment of 4 functions in determining the inverse matrix
## 'cacheSolve' will determine the inverse if it has not been cached
## previously, or will retrieve the cache value if has been determined
## before

## The 'makeCacheMatrix' takes a matrix arguement and stores the
## the environment of 4 functions in determining the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 'cacheSolve' will determine the inverse if it has not been cached
## previously, or will retrieve the cache value if has been determined
## before

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
