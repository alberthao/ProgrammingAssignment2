## This ia a function shows what we can do to same time by cache time-consuming computations.
## <<- operator is used here to assign a value to an object in an enviroment 
## that is different from the current enviroment.
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
  ## matrixget function
  getcache <- function() m
  ## set list of function name for later use
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}
cacheSolve <- function(x, ...) {
  ##get solve matrix cache
  m <- x$getcache()
  ##if cache exist, return "getting cached data" before the matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##if cache not exist, calculate solve of the set matrix and send result to cache
  data <- x$get()
  m <- solve(data,...)
  x$setcache(m)
  m
}
