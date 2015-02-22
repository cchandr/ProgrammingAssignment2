## This function calculates the inverse of a matrix.If the inverse of 
## matrix  has been calculated then inverse is  returned from cache else its calculated and returned
## 

## Returns a list of 4 functions to set and get matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Computes inverse if not present and set the inverse matrix parameter.
## If already calculated then, inverse is returned from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		m <- x$getinv()
		  if(!is.null(m)) {
			message("getting cached data")
			return(m)
		  }
		  data <- x$get()
		  m <- solve(data, ...)
		  x$setinv(m)
		  m
}
