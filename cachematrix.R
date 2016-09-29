##This is a pair of function that can cache the inverse of a matrix

##Creates the matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m<<-inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



##CacheSolve computes the inverse of the matrix. If the matrix is already is the cache, the function retrieve 
##the function from the cache

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
