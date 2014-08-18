

## Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {              ## Input the matrix
    x <<- y
    m <<-  NULL
  }
  get <- function() x               ## Return the matrix
  setInverseM <- function(InverseM) m <<- InverseM       ## Caches the inverse
  getInverseM <- function() m                            ## Returns the cached value
  list(set = set, get = get,
       setInverseM = setInverseM,
       getInverseM = getInverseM)

}


## Checks to see if the matrix has an inverse cached. If not, it returns the inverse and caches it

cacheSolve <- function(x, ...) {
  m <- x$getInverseM()
  
  if(!is.null(m)) {          ## Check if the matrix has an inverse cached
    message("getting cached data")
    return(m)                ## Return the cached inverse
  }
  data <- x$get()            
  m <- solve(data, ...)           ## Caculate the inverse of 'x'
  x$setInverseM(m)            ## Cache the inverse
  m                          ## Return a matrix that is the inverse of 'x'
}
