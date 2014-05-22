makeCacheMatrix <- function(x = matrix()) {
  ##initialize
  m <- NULL
  ## matrix set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## matrix get function
  get <- function() x
  ## setcache function
  setcache <- function(mymatrix) m <<- mymatrix
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
