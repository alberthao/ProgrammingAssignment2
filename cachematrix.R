makeCacheMatrix <- function(x = matrix()) {
  ##initialize
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcache <- function(mean) m <<- mean
  getcache <- function() m
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}
cacheSolve <- function(x, ...) {
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setcache(m)
  m
}
