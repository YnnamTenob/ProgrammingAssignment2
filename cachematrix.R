

## Accept some matrix 'x' as the argument to make another   
## object that stores a cached value of the matrix 
## defined by the setCache function
makeCacheMatrix <- function(m = matrix()) {
  cache <- NULL
  set <- function(y) {
    m <<- y
    cache <<- NULL
  }
  get <- function() m
  setCache <- function(s) cache <<- s
  getCache <- function() cache
  list(set = set, 
       get = get,
       setCache = setCache,
       getCache = getCache)
}


## if the cache is empty 
## set the cache to the inverse of some Cache Matrix object  
## as defined by the makeCacheMatrix function, 
## otherwise get the cached inverse and return the inverse 
cacheSolve <- function(x, ...) {
        
  i <- x$getCache()
  if(!is.null(i)) {
    message("getting cached data")
  }
  else
  {
    i <- solve(x$get(), ...)
    x$setCache(i)
  }
  i
}
